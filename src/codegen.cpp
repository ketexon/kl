#include "codegen.hpp"
#include "parse.hpp"
#include "types.hpp"
#include <expected>

#include <llvm/ADT/APInt.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/raw_ostream.h>

#include <print>
#include <unordered_map>

namespace kl {
namespace codegen {

CodegenError::CodegenError(Type type, std::string message)
    : Error{Stage::Codegen}, type{type}, message{message} {
  error_what = std::format("{}", *this);
}

struct Scope {
  std::unordered_map<std::string, llvm::Value *> identifiers;
};

struct CodegenContext {
  std::shared_ptr<llvm::LLVMContext> llvm_context;
  std::unique_ptr<llvm::Module> module;
  std::unique_ptr<llvm::IRBuilder<>> ir_builder;

  const types::FunctionTypeInfo *cur_function_type_info = nullptr;
  llvm::Function *cur_function_value = nullptr;

  std::vector<Scope> scopes;

  std::unordered_map<std::string, llvm::Value *> static_strings;

  CodegenContext(std::shared_ptr<llvm::LLVMContext> llvm_context)
      : llvm_context{llvm_context} {
    module = std::make_unique<llvm::Module>("module", *llvm_context);
    ir_builder = std::make_unique<llvm::IRBuilder<>>(*llvm_context);
    scopes.emplace_back();
  }

  [[noreturn]] void throw_type_error(std::string message) {
    throw CodegenError{
        CodegenError::Type::TypeError,
        message,
    };
  }

  std::optional<llvm::Value *>
  try_get_identifier(const std::string &name) const {
    for (const auto &scope : std::views::reverse(scopes)) {
      auto it = scope.identifiers.find(name);
      if (it != scope.identifiers.end()) {
        return it->second;
      }
    }
    return std::nullopt;
  }

  llvm::Value *get_identifier(const std::string &name) const {
    auto opt = try_get_identifier(name);
    if (opt.has_value()) {
      return opt.value();
    }
    throw CodegenError{
        CodegenError::Type::UndeclaredIdentifier,
        std::format("Identifier {:?} not declared.", name),
    };
  }

  llvm::Value *create_global_string(const std::string &string) {
    auto it = static_strings.find(string);
    if (it != static_strings.end()) {
      return it->second;
    }
    return ir_builder->CreateGlobalString(string);
  }

  void verify_function(llvm::Function *function) {
    std::string error_string;
    llvm::raw_string_ostream error_ostream{error_string};
    if (llvm::verifyFunction(*function, &error_ostream)) {
      throw CodegenError{
          CodegenError::Type::VerificationError,
          error_string,
      };
    }
  }

  void verify_module() {
    std::string error_string;
    llvm::raw_string_ostream error_ostream{error_string};
    if (llvm::verifyModule(*module, &error_ostream)) {
      throw CodegenError{
          CodegenError::Type::VerificationError,
          error_string,
      };
    }
  }
};

using NodeCodeGenerator = llvm::Value *(*)(CodegenContext &, const ast::Node &);

llvm::Value *codegen_expression(CodegenContext &ctx,
                                const ast::ExpressionNode &expression);

llvm::Value *
codegen_binop(CodegenContext &ctx,
              const ast::BinaryOperatorExpressionNode &binop_expression) {
  auto left = codegen_expression(ctx, *binop_expression.left);
  auto right = codegen_expression(ctx, *binop_expression.right);

  if (!left || !right) {
    return nullptr;
  }

  switch (binop_expression.op) {
  case ast::BinaryOperator::Add:
  case ast::BinaryOperator::Subtract:
  case ast::BinaryOperator::Multiply:
  case ast::BinaryOperator::Divide:
    break;
  }
  return nullptr;
}

llvm::Value *
codegen_function_call_expr(CodegenContext &ctx,
                           const ast::FunctionCallExpressionNode &fcall) {
  auto function_value = codegen_expression(ctx, *fcall.function);
  assert(function_value != nullptr);
  std::vector<llvm::Value *> args;
  for (const auto &arg_node : fcall.arguments) {
    args.push_back(codegen_expression(ctx, *arg_node));
  }

  auto function = llvm::dyn_cast<llvm::Function>(function_value);
  if (!function) {
    ctx.throw_type_error(
        std::format("Tried to call value of type {}",
                    static_cast<int>(function_value->getType()->getTypeID())));
  }

  return ctx.ir_builder->CreateCall(function, args, "calltmp");
}

llvm::Value *codegen_identifier(CodegenContext &ctx,
                                const ast::IdentifierNode &identifier) {
  return ctx.get_identifier(identifier.name);
}

llvm::Value *
codegen_identifier_expr(CodegenContext &ctx,
                        const ast::IdentifierExpressionNode &identifier_expr) {
  return codegen_identifier(ctx, *identifier_expr.identifier);
}

llvm::Value *
codegen_integer_expr(CodegenContext &ctx,
                     const ast::IntegerConstantExpressionNode &integer_expr) {
  auto integer_type = integer_expr.get_integer_type();
  auto llvm_type = integer_type->get_llvm_type(*ctx.llvm_context);
  if (!llvm::ConstantInt::isValueValidForType(llvm_type, integer_expr.value)) {
    throw CodegenError{
        CodegenError::Type::IntegerOutOfBounds,
        std::format(
            "integer {} out of bounds for type {}", integer_expr.value,
            static_cast<const types::TypeInfo &>(*integer_expr.type_info)),
    };
  }
  return llvm::ConstantInt::get(llvm_type, integer_expr.value,
                                integer_expr.get_integer_type()->is_signed);
}

llvm::Value *
codegen_string_expr(CodegenContext &ctx,
                    const ast::StringConstantExpressionNode &string_expr) {
  return ctx.create_global_string(string_expr.value);
}

llvm::Value *codegen_expression(CodegenContext &ctx,
                                const ast::ExpressionNode &expression) {
  switch (expression.type) {
  case ast::NodeType::FunctionCallExpression:
    return codegen_function_call_expr(
        ctx, static_cast<const ast::FunctionCallExpressionNode &>(expression));
  case ast::NodeType::IdentifierExpression:
    return codegen_identifier_expr(
        ctx, static_cast<const ast::IdentifierExpressionNode &>(expression));
  case ast::NodeType::StringConstantExpression:
    return codegen_string_expr(
        ctx,
        static_cast<const ast::StringConstantExpressionNode &>(expression));
  case ast::NodeType::IntegerConstantExpression:
    return codegen_integer_expr(
        ctx,
        static_cast<const ast::IntegerConstantExpressionNode &>(expression));
  default:
    std::println("UNIMPLEMENTED CODEGEN_EXPRESSION: {}", expression.type);
    assert(false);
  }
}

void codegen_expression_statement(
    CodegenContext &ctx, const ast::ExpressionStatementNode &expr_statement) {
  codegen_expression(ctx, *expr_statement.expression);
}

void codegen_return_statement(
    CodegenContext &ctx, const ast::ReturnStatementNode &return_statement) {
  if (return_statement.expression.has_value()) {
    ctx.ir_builder->CreateRet(
        codegen_expression(ctx, *return_statement.expression.value()));
  } else {
    ctx.ir_builder->CreateRetVoid();
  }
}

void codegen_statement(CodegenContext &ctx,
                       const ast::StatementNode &statement) {
  switch (statement.type) {
  case ast::NodeType::ExpressionStatement:
    return codegen_expression_statement(
        ctx, static_cast<const ast::ExpressionStatementNode &>(statement));
  case ast::NodeType::ReturnStatement:
    return codegen_return_statement(
        ctx, static_cast<const ast::ReturnStatementNode &>(statement));
  default:
    std::println("UNIMPLEMENTED CODEGEN_BLOCK: {}", statement.type);
    assert(false);
  }
}

void codegen_block(CodegenContext &ctx, const ast::BlockNode &block) {
  for (const auto &statement : block.statements) {
    codegen_statement(ctx, *statement);
  }
}

void codegen_internal_function_implementation(
    CodegenContext &ctx, const ast::InternalFunctionImplementationNode &impl) {
  auto &block = *impl.block;
  auto basic_block =
      llvm::BasicBlock::Create(*ctx.llvm_context, "", ctx.cur_function_value);
  ctx.ir_builder->SetInsertPoint(basic_block);
  codegen_block(ctx, block);

  auto terminator = basic_block->getTerminator();
  if (!terminator) {
    if (ctx.cur_function_type_info->return_type->is_unit()) {
      ctx.ir_builder->CreateRet(nullptr);
    } else {
      throw CodegenError{
          CodegenError::Type::ReturnTypeMismatch,
          std::format("Expected return type {}, got void",
                      *ctx.cur_function_type_info->return_type),

      };
    }
  }

  ctx.verify_function(ctx.cur_function_value);
}

void codegen_external_function_implementation(
    CodegenContext &ctx, const ast::ExternalFunctionImplementationNode &impl) {
  if (impl.symbol.has_value()) {
    ctx.cur_function_value->setName(impl.symbol.value());
  }
}

void codegen_function_binding_decl(
    CodegenContext &ctx, const ast::BindingDeclarationNode &binding_decl,
    const ast::FunctionDefinitionNode &function_def) {
  auto function_type = static_cast<llvm::FunctionType *>(
      function_def.type_info->get_llvm_type(*ctx.llvm_context));

  auto function =
      llvm::Function::Create(function_type, llvm::Function::ExternalLinkage,
                             binding_decl.identifier->name, ctx.module.get());

  ctx.cur_function_value = function;
  ctx.cur_function_type_info = static_cast<const types::FunctionTypeInfo *>(
      function_def.type_info.get());
  if (function_def.implementation->type ==
      ast::NodeType::InternalFunctionImplementation) {
    const auto& impl = static_cast<const ast::InternalFunctionImplementationNode &>(
        *function_def.implementation);
    codegen_internal_function_implementation(ctx, impl);
  } else if(function_def.implementation->type ==
      ast::NodeType::ExternalFunctionImplementation) {
    const auto& impl = static_cast<const ast::ExternalFunctionImplementationNode &>(
        *function_def.implementation);
    codegen_external_function_implementation(ctx, impl);
  }
}

void codegen_binding_decl(CodegenContext &ctx,
                          const ast::BindingDeclarationNode &binding_decl) {
  switch (binding_decl.binding->type) {
  case ast::NodeType::FunctionDefinition:
    codegen_function_binding_decl(
        ctx, binding_decl,
        static_cast<const ast::FunctionDefinitionNode &>(
            *binding_decl.binding));
    break;
  default:
    std::println("UNIMPLEMENTED BINDING DECL: {}", binding_decl.binding->type);
    assert(false);
  }
}

void codegen_builtins(CodegenContext &ctx) {
}

void codegen_program(CodegenContext &ctx, const ast::ProgramNode &node) {
  codegen_builtins(ctx);

  const auto &program = static_cast<const ast::ProgramNode &>(node);
  for (const auto &decl : program.declarations) {
    switch (decl->type) {
    case ast::NodeType::BindingDeclaration:
      codegen_binding_decl(
          ctx, static_cast<const ast::BindingDeclarationNode &>(*decl));
      break;
    default:
      std::println("UNIMPLEMENTED PROGRAM: {}", decl->type);
      assert(false);
    }
  }

  ctx.verify_module();
}

llvm::Value *codegen_float(CodegenContext &ctx, const ast::Node &node) {
  const auto &program =
      static_cast<const ast::FloatConstantExpressionNode &>(node);
  assert(false);
}

CodegenResult try_codegen(std::shared_ptr<llvm::LLVMContext> llvm_context,
                          const std::unique_ptr<ast::ProgramNode> &program) {
  CodegenContext ctx{std::move(llvm_context)};
  try {
    codegen_program(ctx, *program);
    return Module{
        std::move(ctx.llvm_context),
        std::move(ctx.module),
    };

  } catch (const CodegenError &error) {
    return std::unexpected{error};
  }
}

} // namespace codegen
} // namespace kl
