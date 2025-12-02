#include "parse.hpp"
#include "types.hpp"
#include <memory>
#include <typecheck.hpp>

namespace kl {
namespace ast {

void ProgramNode::typecheck(struct kl::ast::TypecheckContext &ctx) {
  ctx.type_system = std::make_unique<types::TypeSystem>();
  ctx.global_scope = std::make_unique<types::Scope>();

  for (const auto &decl : declarations) {
    decl->typecheck(ctx);
  }
}

void BindingDeclarationNode::typecheck(struct kl::ast::TypecheckContext &ctx) {
  auto current_scope = ctx.get_current_scope();
  if (current_scope->identifiers.contains(identifier->name)) {
    throw types::TypeError{
        types::TypeError::Type::DuplicateBinding,
        std::format("{} bound to multiple values", identifier->name),
    };
  }
  binding->typecheck(ctx);

  current_scope->identifiers[identifier->name] = binding->type_info->clone();
}

void FunctionDefinitionNode::typecheck(TypecheckContext &ctx) {
  std::vector<std::unique_ptr<types::TypeInfo>> argument_types;
  for (const auto &arg : arguments) {
    arg->typecheck(ctx);
    argument_types.push_back(arg->argument_type->type_info->clone());
  }

  std::unique_ptr<types::TypeInfo> return_type_info;
  if (return_type.has_value()) {
    return_type.value()->typecheck(ctx);
    return_type_info = return_type.value()->type_info->clone();
  } else {
    return_type_info = std::make_unique<types::UnitTypeInfo>();
  }

  auto function_type_info = std::make_unique<types::FunctionTypeInfo>(
      std::move(return_type_info), std::move(argument_types));

  type_info = function_type_info->clone();

  std::unique_ptr<types::Scope> old_function_scope =
      std::move(ctx.function_scope);
  std::unique_ptr<types::FunctionTypeInfo> old_function_type_info =
      std::move(ctx.function_type_info);

  ctx.function_scope = std::make_unique<types::Scope>();
  ctx.function_type_info = std::move(function_type_info);

  block->typecheck(ctx);

  ctx.function_scope = std::move(old_function_scope);
  ctx.function_type_info = std::move(old_function_type_info);
}

void BlockNode::typecheck(struct kl::ast::TypecheckContext &ctx) { assert(false); }

void BinaryOperatorExpressionNode::typecheck(TypecheckContext &ctx) {
  left->typecheck(ctx);
  right->typecheck(ctx);

  auto &left_type = left->type_info;
  auto &right_type = left->type_info;
  if (*left_type != *right_type) {
    throw types::TypeError{
        types::TypeError::Type::InvalidOperand,
        std::format("Invalid operands for binary operator {}: {} and {}", op,
                    left_type->to_string(), right_type->to_string()),
    };
  }

  type_info = left_type->clone();
}

static std::unordered_map<std::string, types::FloatTypeInfo> float_suffix_types{
    {"F16", types::FloatTypeInfo{16}},
    {"F32", types::FloatTypeInfo{32}},
    {"F64", types::FloatTypeInfo{64}},
};

static types::FloatTypeInfo default_float_type{32};

static std::unordered_map<std::string, types::IntegerTypeInfo>
    integer_suffix_types{
        {"I8", types::IntegerTypeInfo{8, true}},
        {"I16", types::IntegerTypeInfo{16, true}},
        {"I32", types::IntegerTypeInfo{32, true}},
        {"I64", types::IntegerTypeInfo{64, true}},

        {"U8", types::IntegerTypeInfo{8, false}},
        {"U16", types::IntegerTypeInfo{16, false}},
        {"U32", types::IntegerTypeInfo{32, false}},
        {"U64", types::IntegerTypeInfo{64, false}},
    };

static types::IntegerTypeInfo default_integer_type{32, true};

void FloatConstantExpressionNode::typecheck(TypecheckContext &ctx) {
  type_info = suffix->type_info->clone();
  if (type_info->type != types::TypeInfoType::Float) {
    throw types::TypeError{
        types::TypeError::Type::InvalidFloatSuffix,
        std::format("Invalid suffix for float: {}", *type_info)};
  }
}

void FunctionCallExpressionNode::typecheck(TypecheckContext &ctx) {
  function->typecheck(ctx);

  const auto &uncasted_function_type_info = *function->type_info;
  if (uncasted_function_type_info.type != types::TypeInfoType::Function) {
    throw types::TypeError{
        types::TypeError::Type::InvalidOperand,
        std::format("Tried to call value of type {}", uncasted_function_type_info)};
  }
  const auto &function_type_info =
      static_cast<const types::FunctionTypeInfo &>(uncasted_function_type_info);

  bool is_variadic = function_type_info.is_variadic;
  size_t n_args = arguments.size();
  size_t n_args_expected = function_type_info.argument_types.size();

  if (!is_variadic && n_args != n_args_expected) {
    throw types::TypeError{
        types::TypeError::Type::InvalidOperand,
        std::format("Invalid number of arguments to function call of type {}. "
                    "Expected {}, got {}",
                    uncasted_function_type_info, n_args_expected, n_args)};
  } else if (is_variadic && n_args < n_args_expected) {
    throw types::TypeError{
        types::TypeError::Type::InvalidOperand,
        std::format("Invalid number of arguments to function call of type {}. "
                    "Expected at least {}, got {}",
                    uncasted_function_type_info, n_args_expected, n_args)};
  }

  for (size_t i = 0; i < n_args; ++i) {
    auto &arg_node = arguments[i];
    arg_node->typecheck(ctx);

    auto &expected_type = function_type_info.argument_types[i];

    if (*arg_node->type_info != *expected_type) {
      throw types::TypeError{
          types::TypeError::Type::IncompatibleType,
          std::format("Invalid argument at index {} passed to function {}. "
                      "Expected {}, got {}",
                      i, function->format(0), *arg_node->type_info,
                      *expected_type)};
    }
  }

  type_info = function_type_info.return_type->clone();
}

void IdentifierExpressionNode::typecheck(TypecheckContext &ctx) {
  assert(false);
}

void IntegerConstantExpressionNode::typecheck(TypecheckContext &ctx) { assert(false); }

void StringConstantExpressionNode::typecheck(TypecheckContext &ctx) { assert(false); }

void UnaryOperatorExpressionNode::typecheck(TypecheckContext &ctx) { assert(false); }

void ExpressionStatementNode::typecheck(struct kl::ast::TypecheckContext &ctx) {
}
void FunctionDefinitionArgumentNode::typecheck(
    struct kl::ast::TypecheckContext &ctx) { assert(false); }
void IdentifierNode::typecheck(struct kl::ast::TypecheckContext &ctx) { assert(false); }
void ReturnStatementNode::typecheck(struct kl::ast::TypecheckContext &ctx) { assert(false); }

void TypeNode::typecheck(struct kl::ast::TypecheckContext &ctx) {
  type_info = ctx.type_system->get_type(name)->clone();
}

TypecheckResult try_typecheck(std::unique_ptr<ast::ProgramNode> &&program) {
  ast::TypecheckContext ctx;
  try {
    program->typecheck(ctx);
    return std::move(program);
  } catch (const TypecheckError &error) {
    return std::unexpected{error};
  }
}

} // namespace ast
} // namespace kl
