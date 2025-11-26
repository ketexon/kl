#include "parse.hpp"
#include <memory>

#include <ranges>
#include <unordered_map>
#include <vector>

#include <llvm/ADT/APFloat.h>
#include <llvm/ADT/SmallString.h>

namespace kl {
namespace ast {

Type::Type(std::string name) : name{name} {}

BindingDeclaration::BindingDeclaration(std::unique_ptr<Identifier> identifier,
                                       std::unique_ptr<Binding> &&binding)
    : identifier{std::move(identifier)}, binding{std::move(binding)} {}

BindingDeclaration::~BindingDeclaration() = default;
Statement::Statement() = default;

Program::Program(
    std::vector<std::unique_ptr<TopLevelDeclaration>> &&declarations)
    : declarations{std::move(declarations)} {}

FunctionDefinitionArgument::FunctionDefinitionArgument(
    std::unique_ptr<Identifier> &&identifier, std::unique_ptr<ast::Type> &&type)
    : identifier{std::move(identifier)}, type{std::move(type)} {}

FunctionDefinition::FunctionDefinition(
    std::vector<std::unique_ptr<FunctionDefinitionArgument>> &&arguments,
    std::optional<std::unique_ptr<ast::Type>> &&return_type,
    std::unique_ptr<Block> &&block)
    : arguments{std::move(arguments)}, return_type{std::move(return_type)},
      block{std::move(block)} {}

Block::Block(std::vector<std::unique_ptr<Statement>> &&statements)
    : statements{std::move(statements)} {}

Binding::Binding() = default;

ExpressionStatement::ExpressionStatement(
    std::unique_ptr<Expression> &&expression)
    : expression{std::move(expression)} {}

FunctionCallExpression::FunctionCallExpression(
    std::unique_ptr<Expression> &&function,
    std::vector<std::unique_ptr<Expression>> &&args)
    : function{std::move(function)}, arguments{std::move(args)} {}

UnaryOperatorExpression::UnaryOperatorExpression(
    UnaryOperator op, std::unique_ptr<Expression> &&expr)
    : op{op}, expression{std::move(expr)} {}

BinaryOperatorExpression::BinaryOperatorExpression(
    BinaryOperator op, std::unique_ptr<Expression> &&left,
    std::unique_ptr<Expression> &&right)
    : op{op}, left{std::move(left)}, right{std::move(right)} {}

IdentifierExpression::IdentifierExpression(
    std::unique_ptr<Identifier> &&identifier)
    : identifier{std::move(identifier)} {}

FloatConstantExpression::FloatConstantExpression(std::string value)
    // : value{llvm::APFloatBase::IEEEdouble(), value} {}
    : value{0.0} {}

IntegerConstantExpression::IntegerConstantExpression(std::string value)
    // : value{llvm::APInt::getBitsNeeded(value, 10), value, 10} {}
    : value{llvm::APInt::getBitsNeeded(value, 10), value, 10} {}

StringConstantExpression::StringConstantExpression(std::string value)
    : value{value} {}

ConstantExpression::ConstantExpression() = default;

/** FORMATTING **/

std::string Type::format(std::size_t indent) const {
  return make_indent(indent) + name;
};

std::string BindingDeclaration::format(std::size_t indent) const {
  return std::format("{}{} = {}", make_indent(indent),
                     identifier ? identifier->format(0) : "<nullptr>",
                     binding ? binding->format(0) : "<nullptr>");
};

std::string Program::format(std::size_t indent) const {
  return declarations | std::views::transform([indent](const auto &decl) {
           return decl->format(indent);
         }) |
         std::views::join_with(std::string("\n\n")) |
         std::ranges::to<std::string>();
}

std::string FunctionDefinitionArgument::format(std::size_t indent) const {
  return std::format("{}{}: {}", make_indent(indent),
                     identifier ? identifier->format(0) : "<nullptr>",
                     type ? type->format(0) : "<nullptr>");
}

std::string Identifier::format(std::size_t indent) const {
  return make_indent(indent) + name;
}

std::string FunctionDefinition::format(std::size_t indent) const {
  return std::format(
      "{}fn({}){} {}", make_indent(indent),
      arguments | std::views::transform([](const auto &arg) {
        return arg->format(0);
      }) | std::views::join_with(std::string(", ")) |
          std::ranges::to<std::string>(),
      return_type.has_value()
          ? std::format(": {}",
                        *return_type ? (*return_type)->format(0) : "<nullptr>")
          : "",
      block ? block->format(0) : "<nullptr>"

  );
}

std::string Block::format(std::size_t indent) const {
  if (statements.size() == 0) {
    return make_indent(indent) + "{}";
  }
  return std::format(
      "{}{{\n{}\n}}", make_indent(indent),
      statements | std::views::transform([indent](const auto &statement) {
        return statement->format(indent + 1);
      }) | std::views::join_with(std::string("\n")) |
          std::ranges::to<std::string>());
}

std::string ExpressionStatement::format(std::size_t indent) const {
  return expression ? expression->format(indent) : "<nullptr>";
}

std::string FunctionCallExpression::format(std::size_t indent) const {
  return std::format(
      "{}{}({})", make_indent(indent),
      function ? function->format(0) : "<nullptr>",
      arguments | std::views::transform([](const auto &argument) {
        return argument->format(0);
      }) | std::views::join_with(std::string(", ")) |
          std::ranges::to<std::string>());
}

std::unordered_map<UnaryOperator, const char *> unary_operator_map{
    {UnaryOperator::Negative, "-"},
    {UnaryOperator::Not, "not "},
};

std::unordered_map<BinaryOperator, const char *> binary_operator_map{
    {BinaryOperator::Add, "+"},
    {BinaryOperator::Subtract, "-"},
    {BinaryOperator::Multiply, "* "},
    {BinaryOperator::Divide, "/"},
};

std::string UnaryOperatorExpression::format(std::size_t indent) const {
  return std::format("{}{}{}", make_indent(indent), unary_operator_map.at(op),
                     expression ? expression->format(0) : "<nullptr>");
}

std::string BinaryOperatorExpression::format(std::size_t indent) const {

  return std::format(
      "{}({}{}{})", make_indent(indent), left ? left->format(0) : "<nullptr>",
      binary_operator_map.at(op), right ? right->format(0) : "<nullptr>");
}

std::string IdentifierExpression::format(std::size_t indent) const {
  return identifier ? identifier->format(indent) : "<nullptr>";
}

std::string FloatConstantExpression::format(std::size_t indent) const {
  // llvm::SmallString<32> str;
  // value.toString(str);
  // return std::format("{}{}", make_indent(indent), str.c_str());
  return "TMPFLOAT";
}

std::string IntegerConstantExpression::format(std::size_t indent) const {
  // llvm::SmallString<32> str;
  // value.toString(str, 10, true);
  // return std::format("{}{}", make_indent(indent), str.c_str());
  return "TMPINT";
}

std::string StringConstantExpression::format(std::size_t indent) const {
  return std::format("{}\"{}\"", make_indent(indent), value);
}

} // namespace ast
} // namespace kl
