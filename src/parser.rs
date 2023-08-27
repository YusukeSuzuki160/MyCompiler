use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, char, digit1, multispace0, multispace1, one_of},
    combinator::{map, opt},
    multi::many0,
    sequence::{delimited, preceded, separated_pair, tuple},
    IResult,
};
use std::convert::{TryFrom, TryInto};

const CHAR_SET: &str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";

#[derive(Debug, PartialEq)]
pub enum NumType {
    Int(i32),
    Float(f32),
    Char(char),
    Variable(String),
}

impl NumType {
    pub fn value(&self) -> String {
        match self {
            NumType::Int(int) => int.to_string(),
            NumType::Float(float) => float.to_string(),
            NumType::Char(char) => char.to_string(),
            NumType::Variable(variable) => variable.to_string(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    Equal,
    NotEqual,
}

#[derive(Debug, PartialEq)]
pub enum FormatSpecifier {
    Int,
    Float,
    String,
    Char,
}

#[derive(Debug, PartialEq)]
pub enum StringPart {
    Char(char),
    Format(FormatSpecifier),
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Num(NumType),
    Operation {
        left: Box<Expression>,
        operator: Operator,
        right: Box<Expression>,
    },
    FunctionCall {
        name: String,
        args: Vec<Expression>,
    },
    StringLiteral(Vec<StringPart>),
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Declaration {
        var_type: String,
        name: String,
        value: Option<Expression>,
    },
    Assignment {
        name: String,
        value: Expression,
    },
    WhileLoop {
        condition: String,
        body: Vec<Statement>,
    },
    IfStatement {
        condition: Expression,
        body: Vec<Statement>,
    },
    ElseIfStatement {
        condition: Expression,
        body: Vec<Statement>,
    },
    ElseStatement {
        body: Vec<Statement>,
    },
    FunctionCall {
        name: String,
        args: Vec<Expression>,
    },
    Return {
        value: Expression,
    },
}

impl TryFrom<Expression> for Statement {
    type Error = &'static str;
    fn try_from(expression: Expression) -> Result<Self, Self::Error> {
        match expression {
            Expression::FunctionCall { name, args } => Ok(Statement::FunctionCall { name, args }),
            _ => Err("Expression is not a function call"),
        }
    }
}

impl TryInto<Expression> for Statement {
    type Error = &'static str;
    fn try_into(self) -> Result<Expression, Self::Error> {
        match self {
            Statement::FunctionCall { name, args } => Ok(Expression::FunctionCall { name, args }),
            _ => Err("Statement is not a function call"),
        }
    }
}

impl Statement {
    pub fn get_var_type(&self) -> Option<&str> {
        match self {
            Statement::Declaration { var_type, .. } => Some(var_type),
            _ => None,
        }
    }
    pub fn get_name(&self) -> Option<&str> {
        match self {
            Statement::Declaration { name, .. } => Some(name),
            Statement::Assignment { name, .. } => Some(name),
            _ => None,
        }
    }
    pub fn get_value(&self) -> Option<&Expression> {
        match self {
            Statement::Declaration { value, .. } => value.as_ref(),
            Statement::Assignment { value, .. } => Some(value),
            _ => None,
        }
    }
    pub fn get_condition(&self) -> Option<&str> {
        match self {
            Statement::WhileLoop { condition, .. } => Some(condition),
            _ => None,
        }
    }
    pub fn get_body(&self) -> Option<&Vec<Statement>> {
        match self {
            Statement::WhileLoop { body, .. } => Some(body),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Function {
    pub function_type: String,
    pub name: String,
    pub args: Vec<Statement>,
    pub body: Vec<Statement>,
}

#[derive(Debug, PartialEq)]
pub enum Program {
    Function(Function),
    Declaration {
        var_type: String,
        name: String,
        value: Option<Expression>,
    },
    PreProcessor {
        name: String,
        value: Option<Expression>,
    },
}

impl TryFrom<Statement> for Program {
    type Error = &'static str;
    fn try_from(statement: Statement) -> Result<Self, Self::Error> {
        match statement {
            Statement::Declaration {
                var_type,
                name,
                value,
            } => Ok(Program::Declaration {
                var_type,
                name,
                value,
            }),
            _ => Err("Statement is not a declaration"),
        }
    }
}

impl TryInto<Statement> for Program {
    type Error = &'static str;
    fn try_into(self) -> Result<Statement, Self::Error> {
        match self {
            Program::Declaration {
                var_type,
                name,
                value,
            } => Ok(Statement::Declaration {
                var_type,
                name,
                value,
            }),
            _ => Err("Program is not a declaration"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct AST {
    pub program: Vec<Program>,
}

fn type_tag(input: &str) -> IResult<&str, &str> {
    alt((tag("int"), tag("float"), tag("char")))(input)
}

fn parse_int(input: &str) -> IResult<&str, NumType> {
    let (input, int) = delimited(multispace0, digit1, multispace0)(input)?;
    let int = int.parse::<i32>().unwrap();
    Ok((input, NumType::Int(int)))
}

fn parse_float(input: &str) -> IResult<&str, NumType> {
    let (input, (int_part, decimal_part)) = delimited(
        multispace0,
        separated_pair(digit1, char('.'), digit1),
        multispace0,
    )(input)?;
    let float = format!("{}.{}", int_part, decimal_part)
        .parse::<f32>()
        .unwrap();
    Ok((input, NumType::Float(float)))
}

fn parse_char(input: &str) -> IResult<&str, NumType> {
    let (input, _) = multispace0(input)?;
    let (input, char) = delimited(char('\''), one_of(CHAR_SET), char('\''))(input)?;
    let (input, _) = multispace0(input)?;
    Ok((input, NumType::Char(char)))
}

fn parse_variable(input: &str) -> IResult<&str, NumType> {
    let (input, variable) = delimited(multispace0, alpha1, multispace0)(input)?;
    Ok((input, NumType::Variable(variable.to_string())))
}

fn parse_value(input: &str) -> IResult<&str, NumType> {
    alt((parse_float, parse_int, parse_char, parse_variable))(input)
}

fn parse_function_call(input: &str) -> IResult<&str, Expression> {
    let (input, (_, name, _, args, _)) = tuple((
        multispace0,
        alpha1,
        multispace0,
        delimited(
            char('('),
            many0(delimited(multispace0, parse_expression, opt(char(',')))),
            char(')'),
        ),
        multispace0,
    ))(input)?;
    Ok((
        input,
        Expression::FunctionCall {
            name: name.to_string(),
            args,
        },
    ))
}

fn parse_paren(input: &str) -> IResult<&str, Expression> {
    let (input, _) = multispace0(input)?;
    let (input, expression) = delimited(char('('), parse_expression, char(')'))(input)?;
    let (input, _) = multispace0(input)?;
    Ok((input, expression))
}

fn parse_factor(input: &str) -> IResult<&str, Expression> {
    alt((
        parse_function_call,
        map(parse_value, Expression::Num),
        parse_paren,
    ))(input)
}

fn parse_term(input: &str) -> IResult<&str, Expression> {
    let operators = alt((
        map(char('*'), |_| Operator::Multiply),
        map(char('/'), |_| Operator::Divide),
    ));
    let (input, (first, rest)) = tuple((parse_factor, opt(tuple((operators, parse_term)))))(input)?;
    let mut left = first;
    if let Some((operator, right)) = rest {
        left = Expression::Operation {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        }
    }
    Ok((input, left))
}

fn parse_expr(input: &str) -> IResult<&str, Expression> {
    let operators = alt((
        map(char('+'), |_| Operator::Add),
        map(char('-'), |_| Operator::Subtract),
    ));
    let (input, (first, rest)) = tuple((parse_term, opt(tuple((operators, parse_expr)))))(input)?;
    let mut left = first;
    if let Some((operator, right)) = rest {
        left = Expression::Operation {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        }
    }
    Ok((input, left))
}

fn parse_comparison(input: &str) -> IResult<&str, Expression> {
    let operators = alt((
        map(tag("=="), |_| Operator::Equal),
        map(tag("!="), |_| Operator::NotEqual),
        map(tag("<="), |_| Operator::LessThanOrEqual),
        map(tag(">="), |_| Operator::GreaterThanOrEqual),
        map(char('<'), |_| Operator::LessThan),
        map(char('>'), |_| Operator::GreaterThan),
    ));
    let (input, (first, rest)) =
        tuple((parse_expr, opt(tuple((operators, parse_comparison)))))(input)?;
    let mut left = first;
    if let Some((operator, right)) = rest {
        left = Expression::Operation {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        }
    }
    Ok((input, left))
}

fn parse_format_specifier(input: &str) -> IResult<&str, FormatSpecifier> {
    let (input, specifier) = alt((
        map(tag("%d"), |_| FormatSpecifier::Int),
        map(tag("%f"), |_| FormatSpecifier::Float),
        map(tag("%s"), |_| FormatSpecifier::String),
        map(tag("%c"), |_| FormatSpecifier::Char),
    ))(input)?;
    Ok((input, specifier))
}

fn parse_string_part(input: &str) -> IResult<&str, StringPart> {
    let (input, _) = multispace0(input)?;
    let (input, component) = alt((
        map(one_of(CHAR_SET), StringPart::Char),
        map(parse_format_specifier, StringPart::Format),
    ))(input)?;
    let (input, _) = multispace0(input)?;
    Ok((input, component))
}

#[test]
fn test_parse_string_part() {
    let (input, component) = parse_string_part("b").unwrap();
    assert_eq!(input, "");
    assert_eq!(component, StringPart::Char('b'));
    let (input, component) = parse_string_part("%d").unwrap();
    assert_eq!(input, "");
    assert_eq!(component, StringPart::Format(FormatSpecifier::Int));
}

fn parse_literal(input: &str) -> IResult<&str, Expression> {
    let (input, _) = multispace0(input)?;
    let (input, components) = delimited(char('"'), many0(parse_string_part), char('"'))(input)?;
    let (input, _) = multispace0(input)?;
    Ok((input, Expression::StringLiteral(components)))
}

fn parse_expression(input: &str) -> IResult<&str, Expression> {
    alt((parse_comparison, parse_literal))(input)
}

fn parse_declaration(input: &str) -> IResult<&str, Statement> {
    let (input, (var_type, _, name, _, value, _)) = tuple((
        type_tag,
        multispace1,
        alpha1,
        multispace0,
        opt(preceded(multispace0, preceded(char('='), parse_expression))),
        multispace0,
    ))(input)?;
    Ok((
        input,
        Statement::Declaration {
            var_type: var_type.to_string(),
            name: name.to_string(),
            value,
        },
    ))
}

fn parse_declaration_statement(input: &str) -> IResult<&str, Statement> {
    let (input, (declaration, _)) = tuple((parse_declaration, char(';')))(input)?;
    Ok((input, declaration))
}

fn parse_assignment(input: &str) -> IResult<&str, Statement> {
    let (input, (name, _, value, _, _)) = tuple((
        alpha1,
        multispace0,
        preceded(char('='), parse_expression),
        multispace0,
        char(';'),
    ))(input)?;
    Ok((
        input,
        Statement::Assignment {
            name: name.to_string(),
            value,
        },
    ))
}

fn parse_while(input: &str) -> IResult<&str, Statement> {
    let (input, (_, _, condition, _, _, _, body, _, _)) = tuple((
        tag("while"),
        multispace0,
        delimited(char('('), alpha1, char(')')),
        multispace0,
        char('{'),
        multispace0,
        parse_statements,
        multispace0,
        char('}'),
    ))(input)?;
    Ok((
        input,
        Statement::WhileLoop {
            condition: condition.to_string(),
            body,
        },
    ))
}

fn parse_if(input: &str) -> IResult<&str, Statement> {
    let (input, (_, _, condition, _, _, _, body, _, _)) = tuple((
        tag("if"),
        multispace0,
        delimited(char('('), parse_expression, char(')')),
        multispace0,
        char('{'),
        multispace0,
        parse_statements,
        multispace0,
        char('}'),
    ))(input)?;
    Ok((input, Statement::IfStatement { condition, body }))
}

fn parse_else_if(input: &str) -> IResult<&str, Statement> {
    let (input, (_, _, condition, _, _, _, body, _, _)) = tuple((
        tag("else if"),
        multispace0,
        delimited(char('('), parse_expression, char(')')),
        multispace0,
        char('{'),
        multispace0,
        parse_statements,
        multispace0,
        char('}'),
    ))(input)?;
    Ok((input, Statement::ElseIfStatement { condition, body }))
}

fn parse_else(input: &str) -> IResult<&str, Statement> {
    let (input, (_, _, _, _, body, _, _, _)) = tuple((
        tag("else"),
        multispace0,
        char('{'),
        multispace0,
        parse_statements,
        multispace0,
        char('}'),
        multispace0,
    ))(input)?;
    Ok((input, Statement::ElseStatement { body }))
}

fn parse_function_call_statement(input: &str) -> IResult<&str, Statement> {
    let (input, (function_call, _)) = tuple((parse_function_call, char(';')))(input)?;
    Ok((input, function_call.try_into().unwrap()))
}

fn parse_return(input: &str) -> IResult<&str, Statement> {
    let (input, (_, _, value, _, _)) = tuple((
        tag("return"),
        multispace0,
        parse_expression,
        multispace0,
        char(';'),
    ))(input)?;
    Ok((input, Statement::Return { value }))
}

fn parse_statement(input: &str) -> IResult<&str, Statement> {
    alt((
        parse_while,
        parse_if,
        parse_else_if,
        parse_else,
        parse_declaration_statement,
        parse_declaration,
        parse_assignment,
        parse_function_call_statement,
        parse_return,
    ))(input)
}

fn parse_statements(input: &str) -> IResult<&str, Vec<Statement>> {
    let (input, statements) = many0(preceded(multispace0, parse_statement))(input)?;
    Ok((input, statements))
}

fn parse_function_declaration(input: &str) -> IResult<&str, Function> {
    let (input, (_, function_type, _, name, _, _, args, _, _, _, _, body, _, _, _)) = tuple((
        multispace0,
        type_tag,
        multispace0,
        alpha1,
        multispace0,
        char('('),
        many0(delimited(multispace0, parse_statement, opt(char(',')))),
        char(')'),
        multispace0,
        char('{'),
        multispace0,
        parse_statements,
        multispace0,
        char('}'),
        multispace0,
    ))(input)?;
    Ok((
        input,
        Function {
            function_type: function_type.to_string(),
            name: name.to_string(),
            args,
            body,
        },
    ))
}

pub fn parse(program: &str) -> IResult<&str, AST> {
    let (input, program) = many0(alt((
        map(parse_function_declaration, Program::Function),
        map(parse_declaration, |declaration| {
            declaration.try_into().unwrap()
        }),
    )))(program)?;
    Ok((input, AST { program }))
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_parse_value() {
        let (input, value) = parse_value("10").unwrap();
        assert_eq!(input, "");
        assert_eq!(value, NumType::Int(10));
        let (input, value) = parse_value("10.5").unwrap();
        assert_eq!(input, "");
        assert_eq!(value, NumType::Float(10.5));
        let (input, value) = parse_value("'a'").unwrap();
        assert_eq!(input, "");
        assert_eq!(value, NumType::Char('a'));
        let (input, value) = parse_value("x").unwrap();
        assert_eq!(input, "");
        assert_eq!(value, NumType::Variable("x".to_string()));
    }

    #[test]
    fn test_parse_expression() {
        let (input, expression) = parse_expression("10 + 5").unwrap();
        assert_eq!(input, "");
        assert_eq!(
            expression,
            Expression::Operation {
                left: Box::new(Expression::Num(NumType::Int(10))),
                operator: Operator::Add,
                right: Box::new(Expression::Num(NumType::Int(5))),
            }
        );
        let (input, expression) = parse_expression("10 - 5").unwrap();
        assert_eq!(input, "");
        assert_eq!(
            expression,
            Expression::Operation {
                left: Box::new(Expression::Num(NumType::Int(10))),
                operator: Operator::Subtract,
                right: Box::new(Expression::Num(NumType::Int(5))),
            }
        );
        let (input, expression) = parse_expression("10 * 5").unwrap();
        assert_eq!(input, "");
        assert_eq!(
            expression,
            Expression::Operation {
                left: Box::new(Expression::Num(NumType::Int(10))),
                operator: Operator::Multiply,
                right: Box::new(Expression::Num(NumType::Int(5))),
            }
        );
        let (input, expression) = parse_expression("10 / 5").unwrap();
        assert_eq!(input, "");
        assert_eq!(
            expression,
            Expression::Operation {
                left: Box::new(Expression::Num(NumType::Int(10))),
                operator: Operator::Divide,
                right: Box::new(Expression::Num(NumType::Int(5))),
            }
        );
        let (input, expression) = parse_expression("10 == 5").unwrap();
        assert_eq!(input, "");
        assert_eq!(
            expression,
            Expression::Operation {
                left: Box::new(Expression::Num(NumType::Int(10))),
                operator: Operator::Equal,
                right: Box::new(Expression::Num(NumType::Int(5))),
            }
        );
        let (input, expression) = parse_expression("x + y * z").unwrap();
        assert_eq!(input, "");
        assert_eq!(
            expression,
            Expression::Operation {
                left: Box::new(Expression::Num(NumType::Variable("x".to_string()))),
                operator: Operator::Add,
                right: Box::new(Expression::Operation {
                    left: Box::new(Expression::Num(NumType::Variable("y".to_string()))),
                    operator: Operator::Multiply,
                    right: Box::new(Expression::Num(NumType::Variable("z".to_string()))),
                }),
            }
        );
        let (input, expression) = parse_expression("(x + y) * z").unwrap();
        assert_eq!(input, "");
        assert_eq!(
            expression,
            Expression::Operation {
                left: Box::new(Expression::Operation {
                    left: Box::new(Expression::Num(NumType::Variable("x".to_string()))),
                    operator: Operator::Add,
                    right: Box::new(Expression::Num(NumType::Variable("y".to_string()))),
                }),
                operator: Operator::Multiply,
                right: Box::new(Expression::Num(NumType::Variable("z".to_string()))),
            }
        );
        let (input, expression) = parse_expression("x + y * z + 10").unwrap();
        assert_eq!(input, "");
        assert_eq!(
            expression,
            Expression::Operation {
                left: Box::new(Expression::Num(NumType::Variable("x".to_string()))),
                operator: Operator::Add,
                right: Box::new(Expression::Operation {
                    left: Box::new(Expression::Operation {
                        left: Box::new(Expression::Num(NumType::Variable("y".to_string()))),
                        operator: Operator::Multiply,
                        right: Box::new(Expression::Num(NumType::Variable("z".to_string()))),
                    }),
                    operator: Operator::Add,
                    right: Box::new(Expression::Num(NumType::Int(10))),
                }),
            }
        );
        let (input, expression) = parse_expression("a + b < c * d").unwrap();
        assert_eq!(input, "");
        assert_eq!(
            expression,
            Expression::Operation {
                left: Box::new(Expression::Operation {
                    left: Box::new(Expression::Num(NumType::Variable("a".to_string()))),
                    operator: Operator::Add,
                    right: Box::new(Expression::Num(NumType::Variable("b".to_string()))),
                }),
                operator: Operator::LessThan,
                right: Box::new(Expression::Operation {
                    left: Box::new(Expression::Num(NumType::Variable("c".to_string()))),
                    operator: Operator::Multiply,
                    right: Box::new(Expression::Num(NumType::Variable("d".to_string()))),
                }),
            }
        );
        let (input, expression) = parse_expression("func(x - 1)").unwrap();
        assert_eq!(input, "");
        assert_eq!(
            expression,
            Expression::FunctionCall {
                name: "func".to_string(),
                args: vec![Expression::Operation {
                    left: Box::new(Expression::Num(NumType::Variable("x".to_string()))),
                    operator: Operator::Subtract,
                    right: Box::new(Expression::Num(NumType::Int(1))),
                }],
            }
        );
        let (input, expression) = parse_expression("func(x - 1, y + 1)").unwrap();
        assert_eq!(input, "");
        assert_eq!(
            expression,
            Expression::FunctionCall {
                name: "func".to_string(),
                args: vec![
                    Expression::Operation {
                        left: Box::new(Expression::Num(NumType::Variable("x".to_string()))),
                        operator: Operator::Subtract,
                        right: Box::new(Expression::Num(NumType::Int(1))),
                    },
                    Expression::Operation {
                        left: Box::new(Expression::Num(NumType::Variable("y".to_string()))),
                        operator: Operator::Add,
                        right: Box::new(Expression::Num(NumType::Int(1))),
                    }
                ],
            }
        );
        let (input, expression) = parse_expression("func(x - 1) * 2").unwrap();
        assert_eq!(input, "");
        assert_eq!(
            expression,
            Expression::Operation {
                left: Box::new(Expression::FunctionCall {
                    name: "func".to_string(),
                    args: vec![Expression::Operation {
                        left: Box::new(Expression::Num(NumType::Variable("x".to_string()))),
                        operator: Operator::Subtract,
                        right: Box::new(Expression::Num(NumType::Int(1))),
                    }],
                }),
                operator: Operator::Multiply,
                right: Box::new(Expression::Num(NumType::Int(2))),
            }
        );
        let (input, expression) = parse_expression("func(x - 1) * func(y + 1)").unwrap();
        assert_eq!(input, "");
        assert_eq!(
            expression,
            Expression::Operation {
                left: Box::new(Expression::FunctionCall {
                    name: "func".to_string(),
                    args: vec![Expression::Operation {
                        left: Box::new(Expression::Num(NumType::Variable("x".to_string()))),
                        operator: Operator::Subtract,
                        right: Box::new(Expression::Num(NumType::Int(1))),
                    }],
                }),
                operator: Operator::Multiply,
                right: Box::new(Expression::FunctionCall {
                    name: "func".to_string(),
                    args: vec![Expression::Operation {
                        left: Box::new(Expression::Num(NumType::Variable("y".to_string()))),
                        operator: Operator::Add,
                        right: Box::new(Expression::Num(NumType::Int(1))),
                    }],
                }),
            }
        );
        let (input, expression) = parse_expression(r#""result""#).unwrap();
        assert_eq!(input, "");
        assert_eq!(
            expression,
            Expression::StringLiteral(vec![
                StringPart::Char('r'),
                StringPart::Char('e'),
                StringPart::Char('s'),
                StringPart::Char('u'),
                StringPart::Char('l'),
                StringPart::Char('t'),
            ])
        );
    }
    #[test]
    fn test_parse_statement() {
        let (input, statement) = parse_statement("int x = 10;").unwrap();
        assert_eq!(input, "");
        assert_eq!(
            statement,
            Statement::Declaration {
                var_type: "int".to_string(),
                name: "x".to_string(),
                value: Some(Expression::Num(NumType::Int(10))),
            }
        );
        let (input, statement) = parse_statement("x = x - 1;").unwrap();
        assert_eq!(input, "");
        assert_eq!(
            statement,
            Statement::Assignment {
                name: "x".to_string(),
                value: Expression::Operation {
                    left: Box::new(Expression::Num(NumType::Variable("x".to_string()))),
                    operator: Operator::Subtract,
                    right: Box::new(Expression::Num(NumType::Int(1))),
                }
            }
        );
        let (input, statement) =
            parse_statement("while (x) \n{\n\tx = x - 1;\n\tchar c = 'a';\n }").unwrap();
        assert_eq!(input, "");
        assert_eq!(
            statement,
            Statement::WhileLoop {
                condition: "x".to_string(),
                body: vec![
                    Statement::Assignment {
                        name: "x".to_string(),
                        value: Expression::Operation {
                            left: Box::new(Expression::Num(NumType::Variable("x".to_string()))),
                            operator: Operator::Subtract,
                            right: Box::new(Expression::Num(NumType::Int(1))),
                        },
                    },
                    Statement::Declaration {
                        var_type: "char".to_string(),
                        name: "c".to_string(),
                        value: Some(Expression::Num(NumType::Char('a'))),
                    }
                ],
            }
        );
        let (input, statement) =
            parse_statement("if (x) \n{\n\tx = x - 1;\n\tchar c = 'a';\n }").unwrap();
        assert_eq!(input, "");
        assert_eq!(
            statement,
            Statement::IfStatement {
                condition: Expression::Num(NumType::Variable("x".to_string())),
                body: vec![
                    Statement::Assignment {
                        name: "x".to_string(),
                        value: Expression::Operation {
                            left: Box::new(Expression::Num(NumType::Variable("x".to_string()))),
                            operator: Operator::Subtract,
                            right: Box::new(Expression::Num(NumType::Int(1))),
                        },
                    },
                    Statement::Declaration {
                        var_type: "char".to_string(),
                        name: "c".to_string(),
                        value: Some(Expression::Num(NumType::Char('a'))),
                    }
                ],
            }
        );
        let (input, statement) = parse_statement("return n * factorial(n - 1);").unwrap();
        assert_eq!(input, "");
        assert_eq!(
            statement,
            Statement::Return {
                value: Expression::Operation {
                    left: Box::new(Expression::Num(NumType::Variable("n".to_string()))),
                    operator: Operator::Multiply,
                    right: Box::new(Expression::FunctionCall {
                        name: "factorial".to_string(),
                        args: vec![Expression::Operation {
                            left: Box::new(Expression::Num(NumType::Variable("n".to_string()))),
                            operator: Operator::Subtract,
                            right: Box::new(Expression::Num(NumType::Int(1))),
                        }],
                    }),
                },
            }
        );
    }

    #[test]
    fn test_parse_statements() {
        let input = r#"
        int x= 10;
        float y;
        y = 10.5;
        while (x) {
            x = x - 1;
            char c = 'b';
        }"#;

        let (input, statements) = parse_statements(input).unwrap();
        assert_eq!(input, "");
        assert_eq!(
            statements,
            vec![
                Statement::Declaration {
                    var_type: "int".to_string(),
                    name: "x".to_string(),
                    value: Some(Expression::Num(NumType::Int(10))),
                },
                Statement::Declaration {
                    var_type: "float".to_string(),
                    name: "y".to_string(),
                    value: None,
                },
                Statement::Assignment {
                    name: "y".to_string(),
                    value: Expression::Num(NumType::Float(10.5)),
                },
                Statement::WhileLoop {
                    condition: "x".to_string(),
                    body: vec![
                        Statement::Assignment {
                            name: "x".to_string(),
                            value: Expression::Operation {
                                left: Box::new(Expression::Num(NumType::Variable("x".to_string()))),
                                operator: Operator::Subtract,
                                right: Box::new(Expression::Num(NumType::Int(1))),
                            }
                        },
                        Statement::Declaration {
                            var_type: "char".to_string(),
                            name: "c".to_string(),
                            value: Some(Expression::Num(NumType::Char('b'))),
                        }
                    ],
                }
            ]
        );
    }

    #[test]
    fn test_parse_function_declaration() {
        let input = r#"
        int factorial(int n) {
            if (n == 0) {
                return 1;
            }
            return n * factorial(n - 1);
        }"#;

        let (input, function) = parse_function_declaration(input).unwrap();
        assert_eq!(input, "");
        assert_eq!(
            function,
            Function {
                function_type: "int".to_string(),
                name: "factorial".to_string(),
                args: vec![Statement::Declaration {
                    var_type: "int".to_string(),
                    name: "n".to_string(),
                    value: None,
                }],
                body: vec![
                    Statement::IfStatement {
                        condition: Expression::Operation {
                            left: Box::new(Expression::Num(NumType::Variable("n".to_string()))),
                            operator: Operator::Equal,
                            right: Box::new(Expression::Num(NumType::Int(0))),
                        },
                        body: vec![Statement::Return {
                            value: Expression::Num(NumType::Int(1)),
                        }],
                    },
                    Statement::Return {
                        value: Expression::Operation {
                            left: Box::new(Expression::Num(NumType::Variable("n".to_string()))),
                            operator: Operator::Multiply,
                            right: Box::new(Expression::FunctionCall {
                                name: "factorial".to_string(),
                                args: vec![Expression::Operation {
                                    left: Box::new(Expression::Num(NumType::Variable(
                                        "n".to_string()
                                    ))),
                                    operator: Operator::Subtract,
                                    right: Box::new(Expression::Num(NumType::Int(1))),
                                }],
                            }),
                        },
                    }
                ],
            }
        );
        let input = r#"
        int main() {
            int n = 5;
            int result = factorial(n);
            printf("%d", result);
            return 0;
        }"#;
        let (input, function) = parse_function_declaration(input).unwrap();
        assert_eq!(input, "");
        assert_eq!(
            function,
            Function {
                function_type: "int".to_string(),
                name: "main".to_string(),
                args: vec![],
                body: vec![
                    Statement::Declaration {
                        var_type: "int".to_string(),
                        name: "n".to_string(),
                        value: Some(Expression::Num(NumType::Int(5))),
                    },
                    Statement::Declaration {
                        var_type: "int".to_string(),
                        name: "result".to_string(),
                        value: Some(Expression::FunctionCall {
                            name: "factorial".to_string(),
                            args: vec![Expression::Num(NumType::Variable("n".to_string()))],
                        }),
                    },
                    Statement::FunctionCall {
                        name: "printf".to_string(),
                        args: vec![
                            Expression::StringLiteral(vec![StringPart::Format(
                                FormatSpecifier::Int
                            ),]),
                            Expression::Num(NumType::Variable("result".to_string()))
                        ],
                    },
                    Statement::Return {
                        value: Expression::Num(NumType::Int(0)),
                    }
                ],
            }
        );
    }

    #[test]
    fn test_parse() {
        let input = r#"
        int factorial(int n) {
            if (n == 0) {
                return 1;
            }
            return n * factorial(n - 1);
        }

        int main() {
            int n = 5;
            int result = factorial(n);
            printf("%d", result);
            return 0;
        }
        "#;

        let (input, ast) = parse(input).unwrap();
        assert_eq!(input, "");
        assert_eq!(
            ast,
            AST {
                program: vec![
                    Program::Function(Function {
                        function_type: "int".to_string(),
                        name: "factorial".to_string(),
                        args: vec![Statement::Declaration {
                            var_type: "int".to_string(),
                            name: "n".to_string(),
                            value: None,
                        }],
                        body: vec![
                            Statement::IfStatement {
                                condition: Expression::Operation {
                                    left: Box::new(Expression::Num(NumType::Variable(
                                        "n".to_string()
                                    ))),
                                    operator: Operator::Equal,
                                    right: Box::new(Expression::Num(NumType::Int(0))),
                                },
                                body: vec![Statement::Return {
                                    value: Expression::Num(NumType::Int(1)),
                                }],
                            },
                            Statement::Return {
                                value: Expression::Operation {
                                    left: Box::new(Expression::Num(NumType::Variable(
                                        "n".to_string()
                                    ))),
                                    operator: Operator::Multiply,
                                    right: Box::new(Expression::FunctionCall {
                                        name: "factorial".to_string(),
                                        args: vec![Expression::Operation {
                                            left: Box::new(Expression::Num(NumType::Variable(
                                                "n".to_string()
                                            ))),
                                            operator: Operator::Subtract,
                                            right: Box::new(Expression::Num(NumType::Int(1))),
                                        }],
                                    }),
                                },
                            }
                        ],
                    }),
                    Program::Function(Function {
                        function_type: "int".to_string(),
                        name: "main".to_string(),
                        args: vec![],
                        body: vec![
                            Statement::Declaration {
                                var_type: "int".to_string(),
                                name: "n".to_string(),
                                value: Some(Expression::Num(NumType::Int(5))),
                            },
                            Statement::Declaration {
                                var_type: "int".to_string(),
                                name: "result".to_string(),
                                value: Some(Expression::FunctionCall {
                                    name: "factorial".to_string(),
                                    args: vec![Expression::Num(NumType::Variable("n".to_string()))],
                                }),
                            },
                            Statement::FunctionCall {
                                name: "printf".to_string(),
                                args: vec![
                                    Expression::StringLiteral(vec![StringPart::Format(
                                        FormatSpecifier::Int
                                    ),]),
                                    Expression::Num(NumType::Variable("result".to_string()))
                                ],
                            },
                            Statement::Return {
                                value: Expression::Num(NumType::Int(0)),
                            }
                        ],
                    })
                ],
            }
        );
    }
}
