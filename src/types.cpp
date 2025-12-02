#include "llvm/ADT/APInt.h"
#include <memory>
#include <ranges>
#include <types.hpp>

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/LLVMContext.h>

namespace kl {
namespace types {

TypeInfo::TypeInfo(TypeInfoType ty) : type{ty} {}

std::optional<const TypeInfo *>
Scope::try_get_identifier_type(const std::string &name) const {
  auto it = identifiers.find(name);
  if (it == identifiers.end()) {
    return std::nullopt;
  }
  return it->second.get();
}

void Scope::register_identifier(const std::string &name,
                                std::unique_ptr<TypeInfo> &&type) {
  if (identifiers.contains(name)) {
    throw TypeError{
        TypeError::Type::DuplicateBinding,
        std::format("{} bound to multiple types", name),
    };
  }
}

const TypeInfo *Scope::get_identifier_type(const std::string &name) const {
  auto opt = try_get_identifier_type(name);
  if (opt.has_value()) {
    return opt.value();
  }

  throw TypeError{
      TypeError::Type::UndeclaredIdentifier,
      std::format("undeclared identifier: {}", name),
  };
}

UnitTypeInfo::UnitTypeInfo() : TypeInfo{TypeInfoType::Unit} {}
llvm::Type *UnitTypeInfo::get_llvm_type(llvm::LLVMContext &ctx) const {
  return llvm::Type::getVoidTy(ctx);
}

std::string UnitTypeInfo::to_string() const { return "Unit"; }

std::unique_ptr<TypeInfo> UnitTypeInfo::clone() const {
  return std::make_unique<UnitTypeInfo>();
}

bool UnitTypeInfo::operator==(const TypeInfo &other) const {
  return other.type == type;
}

std::unique_ptr<TypeInfo> IntegerTypeInfo::clone() const {
  return std::make_unique<IntegerTypeInfo>(*this);
}

IntegerTypeInfo::IntegerTypeInfo(size_t num_bits, bool is_signed)
    : TypeInfo{TypeInfoType::Integer}, num_bits{num_bits},
      is_signed{is_signed} {}

llvm::Type *IntegerTypeInfo::get_llvm_type(llvm::LLVMContext &ctx) const {
  return llvm::IntegerType::getIntNTy(ctx, num_bits);
}

std::string IntegerTypeInfo::to_string() const {
  return std::format("{}{}", is_signed ? "I" : "U", num_bits);
}

bool IntegerTypeInfo::operator==(const TypeInfo &other_type) const {
  if (other_type.type != type) {
    return false;
  }
  const auto &other = static_cast<const IntegerTypeInfo &>(other_type);
  return other.is_signed == is_signed && other.num_bits == num_bits;
}

std::unique_ptr<TypeInfo> FloatTypeInfo::clone() const {
  return std::make_unique<FloatTypeInfo>(*this);
}

FloatTypeInfo::FloatTypeInfo(size_t num_bits)
    : TypeInfo{TypeInfoType::Integer}, num_bits{num_bits} {}

llvm::Type *FloatTypeInfo::get_llvm_type(llvm::LLVMContext &ctx) const {
  switch (num_bits) {
  case 16:
    return llvm::Type::getHalfTy(ctx);
  case 32:
    return llvm::Type::getFloatTy(ctx);
  case 64:
    return llvm::Type::getDoubleTy(ctx);
  }
  throw std::format("Unexpected number of bits in float: {}", num_bits);
}

std::string FloatTypeInfo::to_string() const {
  return std::format("F{}", num_bits);
}

bool FloatTypeInfo::operator==(const TypeInfo &other_type) const {
  if (other_type.type != type) {
    return false;
  }
  const auto &other = static_cast<const FloatTypeInfo &>(other_type);
  return other.num_bits == num_bits;
}

FunctionTypeInfo::FunctionTypeInfo(
    std::unique_ptr<TypeInfo> &&return_type,
    std::vector<std::unique_ptr<TypeInfo>> &&args, bool is_variadic)
    : TypeInfo{TypeInfoType::Function}, return_type{std::move(return_type)},
      argument_types{std::move(args)}, is_variadic{is_variadic} {}

FunctionTypeInfo::FunctionTypeInfo(const FunctionTypeInfo &other)
    : FunctionTypeInfo{
          other.return_type->clone(),
          other.argument_types | std::views::transform([](const auto &ty) {
            return ty->clone();
          }) | std::ranges::to<std::vector<std::unique_ptr<TypeInfo>>>(),
          other.is_variadic} {}

std::unique_ptr<TypeInfo> FunctionTypeInfo::clone() const {
  return std::make_unique<FunctionTypeInfo>(*this);
}

llvm::Type *FunctionTypeInfo::get_llvm_type(llvm::LLVMContext &ctx) const {
  std::vector<llvm::Type *> args =
      argument_types | std::views::transform([&ctx](const auto &arg_ty) {
        return arg_ty->get_llvm_type(ctx);
      }) |
      std::ranges::to<std::vector<llvm::Type *>>();
  return llvm::FunctionType::get(return_type->get_llvm_type(ctx), args,
                                 is_variadic);
}

std::string FunctionTypeInfo::to_string() const {
  std::vector<std::string> argument_strings =
      argument_types |
      std::views::transform([](const auto &ty) { return ty->to_string(); }) |
      std::ranges::to<std::vector<std::string>>();
  if (is_variadic) {
    argument_strings.push_back("...");
  }
  return std::format("fn({}): {}",
                     argument_strings |
                         std::views::join_with(std::string(", ")) |
                         std::ranges::to<std::string>(),
                     return_type->to_string());
}

bool FunctionTypeInfo::operator==(const TypeInfo &other_type) const {
  if (other_type.type != type) {
    return false;
  }
  const auto &other = static_cast<const FunctionTypeInfo &>(other_type);
  if (is_variadic != other.is_variadic) {
    return false;
  }
  if (*return_type != *other.return_type) {
    return false;
  }
  auto args_equal =
      std::equal(argument_types.begin(), argument_types.end(),
                 other.argument_types.begin(), other.argument_types.end(),
                 [](const auto &t1, const auto &t2) { return *t1 == *t2; });
  if (!args_equal) {
    return false;
  }
  return true;
}

TypeSystem::TypeSystem() { register_builtin_types(); }

void TypeSystem::register_builtin_types() {
  register_type("I8", std::make_unique<IntegerTypeInfo>(8, true));
  register_type("I16", std::make_unique<IntegerTypeInfo>(16, true));
  register_type("I32", std::make_unique<IntegerTypeInfo>(32, true));
  register_type("I64", std::make_unique<IntegerTypeInfo>(64, true));

  register_type("U8", std::make_unique<IntegerTypeInfo>(8, false));
  register_type("U16", std::make_unique<IntegerTypeInfo>(16, false));
  register_type("U32", std::make_unique<IntegerTypeInfo>(32, false));
  register_type("U64", std::make_unique<IntegerTypeInfo>(64, false));

  register_type("F16", std::make_unique<FloatTypeInfo>(16));
  register_type("F32", std::make_unique<FloatTypeInfo>(32));
  register_type("F64", std::make_unique<FloatTypeInfo>(64));
}

void TypeSystem::register_type(std::string name,
                               std::unique_ptr<TypeInfo> &&type) {
  if (types.contains(name)) {
    throw TypeError{TypeError::Type::DuplicateType,
                    std::format("Duplicate type name: {}", name)};
  }
  types[name] = std::move(type);
}

std::optional<const TypeInfo *>
TypeSystem::try_get_type(const std::string &name) const {
  auto it = types.find(name);
  if (it == types.end()) {
    return std::nullopt;
  }
  return it->second.get();
}

const TypeInfo *TypeSystem::get_type(const std::string &name) const {
  auto opt = try_get_type(name);
  if (opt.has_value()) {
    return opt.value();
  }
  throw TypeError{
      TypeError::Type::UndeclaredType,
      std::format("Type {} was not declared", name),
  };
}

} // namespace types
} // namespace kl
