#include "parse.hpp"
#include "types.hpp"
#include <map>
#include <memory>
#include <print>
#include <tuple>
#include <typecheck.hpp>

namespace kl {
namespace ast {

void ProgramNode::typecheck(ast::TypecheckContext &ctx) {
  ctx.type_system = std::make_unique<types::TypeSystem>();
  ctx.global_scope = std::make_unique<types::Scope>();

  for (const auto &decl : declarations) {
    decl->typecheck(ctx);
  }
}

void BindingDeclarationNode::typecheck(ast::TypecheckContext &ctx) {
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
  std::unique_ptr<types::Scope> old_function_scope =
      std::move(ctx.function_scope);
  ctx.function_scope = std::make_unique<types::Scope>();

  std::vector<std::unique_ptr<types::TypeInfo>> argument_types;
  for (const auto &arg : arguments) {
    std::println("{}", arg->format(0));
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
      std::move(return_type_info), std::move(argument_types), is_variadic);

  type_info = function_type_info->clone();
  

  std::unique_ptr<types::FunctionTypeInfo> old_function_type_info =
      std::move(ctx.function_type_info);
  ctx.function_type_info = std::move(function_type_info);

  implementation->typecheck(ctx);

  ctx.function_scope = std::move(old_function_scope);
  ctx.function_type_info = std::move(old_function_type_info);
}

void InternalFunctionImplementationNode::typecheck(ast::TypecheckContext &ctx) {
  block->typecheck(ctx);
}

void ExternalFunctionImplementationNode::typecheck(ast::TypecheckContext &ctx) {
}

void BlockNode::typecheck(ast::TypecheckContext &ctx) {
  auto old_scope = std::move(ctx.function_scope);
  ctx.function_scope = std::make_unique<types::Scope>();
  ctx.function_scope->parent_scope = old_scope.get();

  for (const auto &stmt : statements) {
    stmt->typecheck(ctx);
  }

  ctx.function_scope = std::move(old_scope);
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
  suffix->typecheck(ctx);
  type_info = suffix->type_info->clone();
  if (type_info->type != types::TypeInfoType::Float) {
    throw types::TypeError{
        types::TypeError::Type::InvalidFloatSuffix,
        std::format("Invalid suffix for float: {}", *type_info)};
  }
}

void IntegerConstantExpressionNode::typecheck(TypecheckContext &ctx) {
  suffix->typecheck(ctx);
  type_info = suffix->type_info->clone();
  if (type_info->type != types::TypeInfoType::Integer) {
    throw types::TypeError{
        types::TypeError::Type::InvalidIntegerSuffix,
        std::format("Invalid suffix for integer: {}", *type_info)};
  }
}

void FunctionCallExpressionNode::typecheck(TypecheckContext &ctx) {
  function->typecheck(ctx);

  const auto &uncasted_function_type_info = *function->type_info;
  if (uncasted_function_type_info.type != types::TypeInfoType::Function) {
    throw types::TypeError{types::TypeError::Type::InvalidOperand,
                           std::format("Tried to call value of type {}",
                                       uncasted_function_type_info)};
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

void IdentifierNode::typecheck(ast::TypecheckContext &ctx) {
  type_info = ctx.get_identifier_type(name)->clone();
}

void IdentifierExpressionNode::typecheck(TypecheckContext &ctx) {
  identifier->typecheck(ctx);
  type_info = identifier->type_info->clone();
}

void StringConstantExpressionNode::typecheck(TypecheckContext &ctx) {
  type_info = std::make_unique<types::StringTypeInfo>();
}

std::map<std::tuple<BinaryOperator, std::unique_ptr<types::TypeInfo>,
                    std::unique_ptr<types::TypeInfo>>,
         std::optional<types::TypeInfoType>>
    binary_operator_types{};

void BinaryOperatorExpressionNode::typecheck(TypecheckContext &ctx) {
  left->typecheck(ctx);
  right->typecheck(ctx);

  auto &left_type = left->type_info;
  auto &right_type = left->type_info;

  if (left_type->is_numeric()) {
    if (*left_type != *right_type) {
      throw types::TypeError{
          types::TypeError::Type::InvalidOperand,
          std::format("Invalid operands for binary operator {}: {} and {}", op,
                      *left_type, *right_type),
      };
    }
    type_info = left_type->clone();
  }
  throw types::TypeError{
      types::TypeError::Type::InvalidOperand,
      std::format("Invalid operands for binary operator {}: {} and {}", op,
                  *left_type, *right_type),
  };
}

void UnaryOperatorExpressionNode::typecheck(TypecheckContext &ctx) {
  expression->typecheck(ctx);
  type_info = expression->type_info->clone();
}

void ExpressionStatementNode::typecheck(ast::TypecheckContext &ctx) {
  expression->typecheck(ctx);
}

void FunctionDefinitionArgumentNode::typecheck(
    ast::TypecheckContext &ctx) {
  argument_type->typecheck(ctx);
}

void ReturnStatementNode::typecheck(ast::TypecheckContext &ctx) {
  std::unique_ptr<types::TypeInfo> return_type_info;
  if (expression) {
    expression.value()->typecheck(ctx);
    return_type_info = expression.value()->type_info->clone();
  } else {
    return_type_info = std::make_unique<types::UnitTypeInfo>();
  }

  auto &expected_return_type = ctx.function_type_info->return_type;
  if (*return_type_info != *expected_return_type) {
    throw types::TypeError{
        types::TypeError::Type::IncompatibleType,
        std::format(
            "Tried to return value of type {} to function that returns {}",
            *return_type_info, *expected_return_type),
    };
  }
}

void TypeNode::typecheck(ast::TypecheckContext &ctx) {
  type_info = ctx.type_system->get_type(name)->clone();
}

TypecheckResult try_typecheck(std::unique_ptr<ast::ProgramNode> &&program) {
  ast::TypecheckContext ctx;
  try {
    program->typecheck(ctx);
    return std::move(program);
  } catch (const types::TypeError &error) {
    return std::unexpected{error};
  }
}

} // namespace ast
} // namespace kl
