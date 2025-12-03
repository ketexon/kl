#include "parse.hpp"
#include <memory>

#include <algorithm>
#include <ostream>
#include <print>
#include <ranges>
#include <unordered_map>
#include <vector>

namespace kl {
namespace ast {

TypeNode::TypeNode(std::string name) : Node{NodeType::Type}, name{name} {}

Node::Node(NodeType type) : type{type} {}

BindingDeclarationNode::BindingDeclarationNode(
    std::unique_ptr<IdentifierNode> identifier,
    std::unique_ptr<BindingNode> &&binding)
    : TopLevelDeclarationNode{NodeType::BindingDeclaration},
      identifier{std::move(identifier)}, binding{std::move(binding)} {}

BindingDeclarationNode::~BindingDeclarationNode() = default;

ProgramNode::ProgramNode(
    std::vector<std::unique_ptr<TopLevelDeclarationNode>> &&declarations)
    : Node{NodeType::Program}, declarations{std::move(declarations)} {}

FunctionDefinitionArgumentNode::FunctionDefinitionArgumentNode(
    std::unique_ptr<IdentifierNode> &&identifier,
    std::unique_ptr<ast::TypeNode> &&type)
    : Node{NodeType::FunctionDefinitionArgument},
      identifier{std::move(identifier)}, argument_type{std::move(type)} {}

FunctionDefinitionNode::FunctionDefinitionNode(
    std::vector<std::unique_ptr<FunctionDefinitionArgumentNode>> &&arguments,
    std::optional<std::unique_ptr<ast::TypeNode>> &&return_type,
    bool is_variadic, std::unique_ptr<FunctionImplementationNode> &&impl)
    : BindingNode{NodeType::FunctionDefinition},
      arguments{std::move(arguments)}, return_type{std::move(return_type)},
      is_variadic{is_variadic}, implementation{std::move(impl)} {}

InternalFunctionImplementationNode::InternalFunctionImplementationNode(
    std::unique_ptr<BlockNode> &&block)
    : FunctionImplementationNode{NodeType::InternalFunctionImplementation},
      block{std::move(block)} {}

ExternalFunctionImplementationNode::ExternalFunctionImplementationNode(
    std::optional<std::string> symbol)
    : FunctionImplementationNode{NodeType::ExternalFunctionImplementation},
      symbol{symbol} {}

BlockNode::BlockNode(std::vector<std::unique_ptr<StatementNode>> &&statements)
    : Node{NodeType::Block}, statements{std::move(statements)} {}

IdentifierNode::IdentifierNode(std::string name)
    : Node{NodeType::Identifier}, name{name} {}

ExpressionStatementNode::ExpressionStatementNode(
    std::unique_ptr<ExpressionNode> &&expression)
    : StatementNode{NodeType::ExpressionStatement},
      expression{std::move(expression)} {}

FunctionCallExpressionNode::FunctionCallExpressionNode(
    std::unique_ptr<ExpressionNode> &&function,
    std::vector<std::unique_ptr<ExpressionNode>> &&args)
    : ExpressionNode{NodeType::FunctionCallExpression},
      function{std::move(function)}, arguments{std::move(args)} {}

UnaryOperatorExpressionNode::UnaryOperatorExpressionNode(
    UnaryOperator op, std::unique_ptr<ExpressionNode> &&expr)
    : ExpressionNode{NodeType::UnaryOperatorExpression}, op{op},
      expression{std::move(expr)} {}

BinaryOperatorExpressionNode::BinaryOperatorExpressionNode(
    BinaryOperator op, std::unique_ptr<ExpressionNode> &&left,
    std::unique_ptr<ExpressionNode> &&right)
    : ExpressionNode{NodeType::BinaryOperatorExpression}, op{op},
      left{std::move(left)}, right{std::move(right)} {}

IdentifierExpressionNode::IdentifierExpressionNode(
    std::unique_ptr<IdentifierNode> &&identifier)
    : ExpressionNode{NodeType::IdentifierExpression},
      identifier{std::move(identifier)} {}

FloatConstantExpressionNode::FloatConstantExpressionNode(
    std::string value, std::unique_ptr<TypeNode> &&type)
    : ConstantExpressionNode{NodeType::FloatConstantExpression}, value{value},
      suffix{std::move(type)} {}

IntegerConstantExpressionNode::IntegerConstantExpressionNode(
    std::uint64_t value, std::unique_ptr<TypeNode> &&type)
    : ConstantExpressionNode{NodeType::IntegerConstantExpression}, value{value},
      suffix{std::move(type)} {}

StringConstantExpressionNode::StringConstantExpressionNode(std::string value)
    : ConstantExpressionNode{NodeType::StringConstantExpression}, value{value} {
}

ReturnStatementNode::ReturnStatementNode(
    std::optional<std::unique_ptr<ExpressionNode>> &&expr)
    : StatementNode{NodeType::ReturnStatement}, expression{std::move(expr)} {}

/** FORMATTING **/

std::string TypeNode::format(std::size_t indent) const {
  return std::format("{}{}", make_indent(indent), name);
};

std::string BindingDeclarationNode::format(std::size_t indent) const {
  return std::format("{}{} = {}", make_indent(indent),
                     identifier ? identifier->format(0) : "<nullptr>",
                     binding ? binding->format(0) : "<nullptr>");
};

std::string ProgramNode::format(std::size_t indent) const {
  return declarations | std::views::transform([indent](const auto &decl) {
           return decl->format(indent);
         }) |
         std::views::join_with(std::string("\n\n")) |
         std::ranges::to<std::string>();
}

std::string FunctionDefinitionArgumentNode::format(std::size_t indent) const {
  return std::format("{}{}: {}", make_indent(indent),
                     identifier ? identifier->format(0) : "<nullptr>",
                     argument_type ? argument_type->format(0) : "<nullptr>");
}

std::string IdentifierNode::format(std::size_t indent) const {
  return make_indent(indent) + name;
}

std::string FunctionDefinitionNode::format(std::size_t indent) const {
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
      implementation ? implementation->format(0) : "<nullptr>"

  );
}

std::string InternalFunctionImplementationNode::format(std::size_t indent) const {
  return block->format(indent);
}

std::string ExternalFunctionImplementationNode::format(std::size_t indent) const {
  return std::format("{}extern{}", make_indent(indent), symbol.has_value() ? std::format(" {:?}", symbol.value()) : "");
}

std::string BlockNode::format(std::size_t indent) const {
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

std::string ExpressionStatementNode::format(std::size_t indent) const {
  return expression ? expression->format(indent) : "<nullptr>";
}

std::string FunctionCallExpressionNode::format(std::size_t indent) const {
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

std::string UnaryOperatorExpressionNode::format(std::size_t indent) const {
  return std::format("{}{}{}", make_indent(indent), unary_operator_map.at(op),
                     expression ? expression->format(0) : "<nullptr>");
}

std::string BinaryOperatorExpressionNode::format(std::size_t indent) const {

  return std::format(
      "{}({}{}{})", make_indent(indent), left ? left->format(0) : "<nullptr>",
      binary_operator_map.at(op), right ? right->format(0) : "<nullptr>");
}

std::string IdentifierExpressionNode::format(std::size_t indent) const {
  return identifier ? identifier->format(indent) : "<nullptr>";
}

std::string FloatConstantExpressionNode::format(std::size_t indent) const {
  return std::format("{}{}{}", make_indent(indent), value,
                     suffix ? suffix->format(0) : "");
}

std::string IntegerConstantExpressionNode::format(std::size_t indent) const {
  return std::format("{}{}{}", make_indent(indent), value,
                     suffix ? suffix->format(0) : "");
}

std::string StringConstantExpressionNode::format(std::size_t indent) const {
  return std::format("{}{:?}", make_indent(indent), value);
}

std::string ReturnStatementNode::format(std::size_t indent) const {
  return std::format(
      "{}return{}", make_indent(indent),
      expression.has_value()
          ? expression.value()
                ? std::format(" {}", expression.value()->format(0))
                : "<nullptr>"
          : "");
}

/** OPERATOR== */

#define KL_AST_OPERATOR_EQ(NodeType)                                           \
  bool NodeType::operator==(const Node &uncasted_node) const {                 \
    if (type != uncasted_node.type) {                                          \
      return false;                                                            \
    }                                                                          \
    const auto &other = static_cast<const NodeType &>(uncasted_node);

#define KL_AST_SUBNODE_EQ(a, b)                                                \
  (static_cast<bool>(a) == static_cast<bool>(b) &&                             \
   (!(a) || static_cast<const Node &>(*a) == static_cast<const Node &>(*b)))

#define KL_AST_SUBNODE_OPT_EQ(a, b)                                            \
  ((a).has_value() == (b).has_value() &&                                       \
   (!(a).has_value() || KL_AST_SUBNODE_EQ(a.value(), b.value())))

#define KL_AST_SUBNODE_RANGE_EQ(as, bs)                                        \
  std::ranges::equal(as, bs, [](const auto &a, const auto &b) {                \
    return KL_AST_SUBNODE_EQ(a, b);                                            \
  })

KL_AST_OPERATOR_EQ(StringConstantExpressionNode)
return value == other.value;
}

KL_AST_OPERATOR_EQ(IntegerConstantExpressionNode)
return value == other.value;
}

KL_AST_OPERATOR_EQ(FloatConstantExpressionNode)
return value == other.value;
}

KL_AST_OPERATOR_EQ(IdentifierExpressionNode)
return KL_AST_SUBNODE_EQ(identifier, other.identifier);
}

KL_AST_OPERATOR_EQ(BinaryOperatorExpressionNode)
return op == other.op && KL_AST_SUBNODE_EQ(left, other.left) &&
       KL_AST_SUBNODE_EQ(right, other.right);
}

KL_AST_OPERATOR_EQ(UnaryOperatorExpressionNode)
return op == other.op && KL_AST_SUBNODE_EQ(expression, other.expression);
}

KL_AST_OPERATOR_EQ(FunctionCallExpressionNode)
return KL_AST_SUBNODE_EQ(function, other.function) &&
       KL_AST_SUBNODE_RANGE_EQ(arguments, other.arguments);
}

KL_AST_OPERATOR_EQ(ExpressionStatementNode)
return KL_AST_SUBNODE_EQ(expression, other.expression);
}

KL_AST_OPERATOR_EQ(BlockNode)
return KL_AST_SUBNODE_RANGE_EQ(statements, other.statements);
}

KL_AST_OPERATOR_EQ(FunctionDefinitionNode)
return KL_AST_SUBNODE_RANGE_EQ(arguments, other.arguments) &&
       KL_AST_SUBNODE_OPT_EQ(return_type, other.return_type) &&
       KL_AST_SUBNODE_EQ(implementation, other.implementation);
}

KL_AST_OPERATOR_EQ(InternalFunctionImplementationNode)
return KL_AST_SUBNODE_EQ(block, other.block);
}

KL_AST_OPERATOR_EQ(ExternalFunctionImplementationNode)
  return symbol == other.symbol;
}

KL_AST_OPERATOR_EQ(IdentifierNode)
return name == other.name;
}

KL_AST_OPERATOR_EQ(FunctionDefinitionArgumentNode)
return KL_AST_SUBNODE_EQ(identifier, other.identifier) &&
       KL_AST_SUBNODE_EQ(argument_type, other.argument_type);
}

KL_AST_OPERATOR_EQ(ProgramNode)
return KL_AST_SUBNODE_RANGE_EQ(declarations, other.declarations);
}

KL_AST_OPERATOR_EQ(TypeNode)
return name == other.name;
}

KL_AST_OPERATOR_EQ(BindingDeclarationNode)
return KL_AST_SUBNODE_EQ(identifier, other.identifier) &&
       KL_AST_SUBNODE_EQ(binding, other.binding);
}

KL_AST_OPERATOR_EQ(ReturnStatementNode)
return KL_AST_SUBNODE_OPT_EQ(expression, other.expression);
}

} // namespace ast
} // namespace kl

std::ostream &operator<<(std::ostream &os, const kl::ast::Node &node) {
  std::print(os, "{}", node.format(0));
  return os;
}
