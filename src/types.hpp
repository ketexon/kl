#pragma once

#include <bit>

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Value.h"
#include <format>
#include <llvm/IR/Type.h>
#include <string>
#include <unordered_map>

namespace kl {
namespace types {

enum class TypeInfoType {
  Unit,
  Integer,
  Float,
  String,
  Function,
};

struct TypeInfo {
  TypeInfo(TypeInfoType ty);
  virtual ~TypeInfo() = default;

  virtual constexpr size_t get_size_bits() const = 0;
  virtual constexpr size_t get_size_bytes() const {
    return std::bit_ceil(get_size_bits()) / 8;
  }
  virtual llvm::Type *get_llvm_type(llvm::LLVMContext &ctx) const = 0;

  virtual std::string to_string() const = 0;
  virtual std::unique_ptr<TypeInfo> clone() const = 0;

  virtual bool operator==(const TypeInfo &) const = 0;

  TypeInfoType type;
};

struct UnitTypeInfo : TypeInfo {
  UnitTypeInfo();
  constexpr size_t get_size_bits() const override { return 0; }
  llvm::Type *get_llvm_type(llvm::LLVMContext &ctx) const override;
  std::string to_string() const override;
  std::unique_ptr<TypeInfo> clone() const override;
  bool operator==(const TypeInfo &) const override;
};

struct IntegerTypeInfo : TypeInfo {
  IntegerTypeInfo(size_t num_bits, bool is_signed);
  IntegerTypeInfo(const IntegerTypeInfo &) = default;
  std::unique_ptr<TypeInfo> clone() const override;

  size_t num_bits;
  bool is_signed;

  constexpr size_t get_size_bits() const override { return num_bits; }

  llvm::Type *get_llvm_type(llvm::LLVMContext &ctx) const override;
  std::string to_string() const override;
  bool operator==(const TypeInfo &) const override;
};

struct FloatTypeInfo : TypeInfo {
  FloatTypeInfo(size_t num_bits);
  FloatTypeInfo(const FloatTypeInfo &) = default;
  std::unique_ptr<TypeInfo> clone() const override;

  size_t num_bits;

  constexpr size_t get_size_bits() const override { return num_bits; }
  llvm::Type *get_llvm_type(llvm::LLVMContext &ctx) const override;
  std::string to_string() const override;
  bool operator==(const TypeInfo &) const override;
};

struct FunctionTypeInfo : TypeInfo {
  FunctionTypeInfo(std::unique_ptr<TypeInfo> &&,
                   std::vector<std::unique_ptr<TypeInfo>> &&,
                   bool is_variadic = false);
  FunctionTypeInfo(const FunctionTypeInfo &);
  std::unique_ptr<TypeInfo> clone() const override;

  std::unique_ptr<TypeInfo> return_type;
  std::vector<std::unique_ptr<TypeInfo>> argument_types;
  bool is_variadic = false;

  constexpr size_t get_size_bits() const override { return 64; }
  llvm::Type *get_llvm_type(llvm::LLVMContext &ctx) const override;
  std::string to_string() const override;
  bool operator==(const TypeInfo &) const override;
};

struct TypeError {
  enum class Type {
    InvalidOperand,
    InvalidFloatSuffix,
    InvalidIntegerSuffix,
    UndeclaredType,
    UndeclaredIdentifier,
    DuplicateType,
    DuplicateBinding,
    IncompatibleType,
  };
  Type type;
  std::string message;

  constexpr static const char *type_to_string(Type ty) {
    switch (ty) {
    case Type::InvalidOperand:
      return "InvalidOperand";
    case Type::InvalidFloatSuffix:
      return "InvalidFloatSuffix";
    case Type::InvalidIntegerSuffix:
      return "InvalidIntegerSuffix";
    case Type::UndeclaredIdentifier:
      return "UndeclaredIdentifier";
    case Type::DuplicateType:
      return "DuplicateType";
    case Type::DuplicateBinding:
      return "DuplicateBinding";
    case Type::IncompatibleType:
      return "IncompatibleType";
    case Type::UndeclaredType:
      return "UndeclaredType";
    }
  }
};

struct Scope {
  Scope *parent_scope = nullptr;
  std::unordered_map<std::string, std::unique_ptr<TypeInfo>> identifiers;

  std::optional<const TypeInfo *>
  try_get_identifier_type(const std::string &) const;
  const TypeInfo *get_identifier_type(const std::string &) const;

  void register_identifier(const std::string &name,
                           std::unique_ptr<TypeInfo> &&type);
};

struct TypeSystem {
  TypeSystem();

  std::unordered_map<std::string, std::unique_ptr<TypeInfo>> types;
  std::unordered_map<std::string, std::unique_ptr<TypeInfo>>
      toplevel_identifier_types;

  void register_type(std::string, std::unique_ptr<TypeInfo> &&);

  std::optional<const TypeInfo *> try_get_type(const std::string &) const;
  const TypeInfo *get_type(const std::string &) const;

protected:
  void register_builtin_types();
};

} // namespace types
} // namespace kl

template <>
struct std::formatter<kl::types::TypeInfo> : std::formatter<std::string> {
  auto format(const kl::types::TypeInfo &type, std::format_context &ctx) const {
    return std::formatter<std::string>::format(type.to_string(), ctx);
  }
};

template <>
struct std::formatter<kl::types::TypeError> : std::formatter<std::string> {
  auto format(const kl::types::TypeError &err, std::format_context &ctx) const {
    return std::formatter<std::string>::format(
        std::format("TypeError({}) {}", err.type_to_string(err.type),
                    err.message),
        ctx);
  }
};
