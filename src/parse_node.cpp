#include "parse.hpp"
#include <memory>

#include <print>
#include <ranges>
#include <unordered_map>
#include <vector>
#include <ostream>

namespace kl {
namespace ast {

static std::unordered_map<std::string, BuiltinType> builtin_type_map {
  { "Unit", BuiltinType::Unit },
};

std::optional<BuiltinType> Type::get_builtin_type(const std::string& s) {
  auto it = builtin_type_map.find(s);
  if (it != builtin_type_map.end()) {
    return it->second;
  }
  return std::nullopt;
}

Type::Type(std::string name) : Node{Node::Type::Type} {
  auto builtin = get_builtin_type(name);
  if (builtin.has_value()) {
    this->name = builtin.value();
  } else {
    this->name = name;
  }
}

Node::Node(Type type) : type{type} {}

BindingDeclaration::BindingDeclaration(std::unique_ptr<Identifier> identifier,
                                       std::unique_ptr<Binding> &&binding)
    : TopLevelDeclaration{Node::Type::BindingDeclaration},
      identifier{std::move(identifier)}, binding{std::move(binding)} {}

BindingDeclaration::~BindingDeclaration() = default;

Program::Program(
    std::vector<std::unique_ptr<TopLevelDeclaration>> &&declarations)
    : Node{Node::Type::Program}, declarations{std::move(declarations)} {}

FunctionDefinitionArgument::FunctionDefinitionArgument(
    std::unique_ptr<Identifier> &&identifier, std::unique_ptr<ast::Type> &&type)
    : Node{Node::Type::FunctionDefinitionArgument},
      identifier{std::move(identifier)}, argument_type{std::move(type)} {}

FunctionDefinition::FunctionDefinition(
    std::vector<std::unique_ptr<FunctionDefinitionArgument>> &&arguments,
    std::optional<std::unique_ptr<ast::Type>> &&return_type,
    std::unique_ptr<Block> &&block)
    : Binding{Node::Type::FunctionDefinition}, arguments{std::move(arguments)},
      return_type{std::move(return_type)}, block{std::move(block)} {}

Block::Block(std::vector<std::unique_ptr<Statement>> &&statements)
    : Node{Node::Type::Block}, statements{std::move(statements)} {}

Identifier::Identifier(std::string name)
    : Node{Node::Type::Identifier}, name{name} {}

ExpressionStatement::ExpressionStatement(
    std::unique_ptr<Expression> &&expression)
    : Statement{Node::Type::ExpressionStatement},
      expression{std::move(expression)} {}

FunctionCallExpression::FunctionCallExpression(
    std::unique_ptr<Expression> &&function,
    std::vector<std::unique_ptr<Expression>> &&args)
    : Expression{Node::Type::FunctionCallExpression},
      function{std::move(function)}, arguments{std::move(args)} {}

UnaryOperatorExpression::UnaryOperatorExpression(
    UnaryOperator op, std::unique_ptr<Expression> &&expr)
    : Expression{Node::Type::UnaryOperatorExpression}, op{op},
      expression{std::move(expr)} {}

BinaryOperatorExpression::BinaryOperatorExpression(
    BinaryOperator op, std::unique_ptr<Expression> &&left,
    std::unique_ptr<Expression> &&right)
    : Expression{Node::Type::BinaryOperatorExpression}, op{op},
      left{std::move(left)}, right{std::move(right)} {}

IdentifierExpression::IdentifierExpression(
    std::unique_ptr<Identifier> &&identifier)
    : Expression{Node::Type::IdentifierExpression},
      identifier{std::move(identifier)} {}

FloatConstantExpression::FloatConstantExpression(std::string value)
    // : value{llvm::APFloatBase::IEEEdouble(), value} {}
    : ConstantExpression{Node::Type::FloatConstantExpression}, value{value} {}

IntegerConstantExpression::IntegerConstantExpression(std::string value)
    // : value{llvm::APInt::getBitsNeeded(value, 10), value, 10} {}
    : ConstantExpression{Node::Type::IntegerConstantExpression}, value{value} {}

StringConstantExpression::StringConstantExpression(std::string value)
    : ConstantExpression{Node::Type::StringConstantExpression}, value{value} {}

/** FORMATTING **/

std::string Type::format(std::size_t indent) const {
  return std::visit([indent](const auto& v) {
    return std::format("{}{}", make_indent(indent), v);
  }, name);
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
                     argument_type ? argument_type->format(0) : "<nullptr>");
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

KL_AST_OPERATOR_EQ(StringConstantExpression)
return value == other.value;
}

KL_AST_OPERATOR_EQ(IntegerConstantExpression)
return value == other.value;
}

KL_AST_OPERATOR_EQ(FloatConstantExpression)
return value == other.value;
}

KL_AST_OPERATOR_EQ(IdentifierExpression)
return KL_AST_SUBNODE_EQ(identifier, other.identifier);
}

KL_AST_OPERATOR_EQ(BinaryOperatorExpression)
return op == other.op && KL_AST_SUBNODE_EQ(left, other.left) &&
       KL_AST_SUBNODE_EQ(right, other.right);
}

KL_AST_OPERATOR_EQ(UnaryOperatorExpression)
return op == other.op && KL_AST_SUBNODE_EQ(expression, other.expression);
}

KL_AST_OPERATOR_EQ(FunctionCallExpression)
return KL_AST_SUBNODE_EQ(function, other.function) &&
       KL_AST_SUBNODE_RANGE_EQ(arguments, other.arguments);
}

KL_AST_OPERATOR_EQ(ExpressionStatement)
return KL_AST_SUBNODE_EQ(expression, other.expression);
}

KL_AST_OPERATOR_EQ(Block)
return KL_AST_SUBNODE_RANGE_EQ(statements, other.statements);
}

KL_AST_OPERATOR_EQ(FunctionDefinition)
return KL_AST_SUBNODE_RANGE_EQ(arguments, other.arguments) &&
       KL_AST_SUBNODE_OPT_EQ(return_type, other.return_type) &&
       KL_AST_SUBNODE_EQ(block, other.block);
}

KL_AST_OPERATOR_EQ(Identifier)
return name == other.name;
}

KL_AST_OPERATOR_EQ(FunctionDefinitionArgument)
return KL_AST_SUBNODE_EQ(identifier, other.identifier) &&
       KL_AST_SUBNODE_EQ(argument_type, other.argument_type);
}

KL_AST_OPERATOR_EQ(Program)
  return KL_AST_SUBNODE_RANGE_EQ(declarations, other.declarations);
}

KL_AST_OPERATOR_EQ(Type)
  return name == other.name;
}

KL_AST_OPERATOR_EQ(BindingDeclaration)
return KL_AST_SUBNODE_EQ(identifier, other.identifier) &&
       KL_AST_SUBNODE_EQ(binding, other.binding);
}

} // namespace ast
} // namespace kl

std::ostream& operator<<(std::ostream& os, const kl::ast::Node& node) {
  std::print(os, "{}", node.format(0));
  return os;
}
