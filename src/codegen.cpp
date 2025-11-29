#include "codegen.hpp"
#include "parse.hpp"
#include <expected>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/LLVMContext.h>

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>
#include <print>

namespace kl {
namespace codegen {

struct Scope {
  std::unordered_map<std::string, llvm::Value *> identifiers;
};

struct CodegenContext {
  std::unique_ptr<llvm::LLVMContext> &llvm_context;
  std::unique_ptr<llvm::Module> module;
  std::unique_ptr<llvm::IRBuilder<>> ir_builder;

  std::vector<Scope> scopes;

  std::unordered_map<std::string, llvm::Value *> static_strings;

  CodegenContext(std::unique_ptr<llvm::LLVMContext> &llvm_context)
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
};

using NodeCodeGenerator = llvm::Value *(*)(CodegenContext &, const ast::Node &);

llvm::Value *codegen_expression(CodegenContext &ctx,
                                const ast::Expression &expression);

llvm::Value *
codegen_binop(CodegenContext &ctx,
              const ast::BinaryOperatorExpression &binop_expression) {
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
                           const ast::FunctionCallExpression &fcall) {
  auto function_value = codegen_expression(ctx, *fcall.function);
  assert(function_value != nullptr);
  std::vector<llvm::Value *> args;
  for (const auto &arg_node : fcall.arguments) {
    args.push_back(codegen_expression(ctx, *arg_node));
  }

  auto function = llvm::dyn_cast<llvm::Function>(function_value);
  if (!function) {
    ctx.throw_type_error(std::format("Tried to call value of type {}",
                                     static_cast<int>(function_value->getType()->getTypeID())));
  }

  return ctx.ir_builder->CreateCall(function, args, "calltmp");
}

llvm::Value *codegen_identifier(CodegenContext &ctx,
                                const ast::Identifier &identifier) {
  return ctx.get_identifier(identifier.name);
}

llvm::Value *
codegen_identifier_expr(CodegenContext &ctx,
                        const ast::IdentifierExpression &identifier_expr) {
  return codegen_identifier(ctx, *identifier_expr.identifier);
}

llvm::Value *
codegen_string_expr(CodegenContext &ctx,
                    const ast::StringConstantExpression &string_expr) {
  return ctx.create_global_string(string_expr.value);
}

llvm::Value *codegen_expression(CodegenContext &ctx,
                                const ast::Expression &expression) {
  switch (expression.type) {
  case ast::Node::Type::FunctionCallExpression:
    return codegen_function_call_expr(
        ctx, static_cast<const ast::FunctionCallExpression &>(expression));
  case ast::Node::Type::IdentifierExpression:
    return codegen_identifier_expr(
        ctx, static_cast<const ast::IdentifierExpression &>(expression));
  case ast::Node::Type::StringConstantExpression:
    return codegen_string_expr(
        ctx, static_cast<const ast::StringConstantExpression &>(expression));
  default:
    std::println("UNIMPLEMENTED CODEGEN_EXPRESSION: {}", expression.type);
    throw "unimplemented";
  }
}

void codegen_block(CodegenContext &ctx, const ast::Block &block) {
  for (const auto &statement : block.statements) {
    switch (statement->type) {
    case ast::Node::Type::ExpressionStatement: {
      const auto &expression_statement =
          static_cast<const ast::ExpressionStatement &>(*statement);
      codegen_expression(ctx, *expression_statement.expression);
      break;
    }
    default:
      std::println("UNIMPLEMENTED CODEGEN_BLOCK: {}", statement->type);
      throw "unimplemented";
    }
  }
}

void codegen_function_binding_decl(
    CodegenContext &ctx, const ast::BindingDeclaration &binding_decl,
    const ast::FunctionDefinition &function_def) {
  std::vector<llvm::Type *> arg_types;
  llvm::Type *return_type = llvm::FunctionType::getVoidTy(*ctx.llvm_context);
  llvm::FunctionType *function_type =
      llvm::FunctionType::get(return_type, arg_types, false);

  auto function =
      llvm::Function::Create(function_type, llvm::Function::ExternalLinkage,
                             binding_decl.identifier->name, ctx.module.get());

  auto basic_block = llvm::BasicBlock::Create(*ctx.llvm_context, "", function);

  ctx.ir_builder->SetInsertPoint(basic_block);
  codegen_block(ctx, *function_def.block);
}

void codegen_binding_decl(CodegenContext &ctx,
                          const ast::BindingDeclaration &binding_decl) {
  switch (binding_decl.binding->type) {
  case ast::Node::Type::FunctionDefinition:
    codegen_function_binding_decl(
        ctx, binding_decl,
        static_cast<const ast::FunctionDefinition &>(*binding_decl.binding));
    break;
  default:
    std::println("UNIMPLEMENTED BINDING DECL: {}", binding_decl.binding->type);
    throw "unimplemented";
  }
}

void codegen_builtins(CodegenContext &ctx) {
  std::vector<llvm::Type *> arg_types;
  arg_types.push_back(llvm::PointerType::get(*ctx.llvm_context, 0));

  llvm::Type *return_type = llvm::FunctionType::getInt32Ty(*ctx.llvm_context);
  llvm::FunctionType *function_type =
      llvm::FunctionType::get(return_type, arg_types, false);

  auto function = llvm::Function::Create(
      function_type, llvm::Function::ExternalLinkage, "puts", ctx.module.get());

  ctx.scopes.back().identifiers["puts"] = function;
}

void codegen_program(CodegenContext &ctx, const ast::Program &node) {
  codegen_builtins(ctx);

  const auto &program = static_cast<const ast::Program &>(node);
  for (const auto &decl : program.declarations) {
    switch (decl->type) {
    case ast::Node::Type::BindingDeclaration:
      codegen_binding_decl(ctx,
                           static_cast<const ast::BindingDeclaration &>(*decl));
      break;
    default:
      std::println("UNIMPLEMENTED PROGRAM: {}", decl->type);
      throw "unimplemented";
    }
  }
}

llvm::Value *codegen_float(CodegenContext &ctx, const ast::Node &node) {
  const auto &program = static_cast<const ast::FloatConstantExpression &>(node);
  throw "unimplemented";
}

CodegenResult try_codegen(std::unique_ptr<llvm::LLVMContext> &llvm_context,
                          const std::unique_ptr<ast::Program> &program) {
  CodegenContext ctx{llvm_context};
  try {
    codegen_program(ctx, *program);
    return std::move(ctx.module);
  } catch (const CodegenError &error) {
    return std::unexpected{error};
  }
}

} // namespace codegen
} // namespace kl
