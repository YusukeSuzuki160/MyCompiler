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
pub enum Expression {
    Num(NumType),
    Operation {
        left: Box<Expression>,
        operator: Operator,
        right: Box<Expression>,
    },
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
    Return {
        value: Expression,
    },
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
    let (input, char) = delimited(char('\''), one_of("a-zA-Z0-9"), char('\''))(input)?;
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

fn parse_expression(input: &str) -> IResult<&str, Expression> {
    let (input, val) = parse_value(input)?;
    let (input, operator) = opt(tuple((
        alt((
            tag("+"),
            tag("-"),
            tag("*"),
            tag("/"),
            tag("<"),
            tag("<="),
            tag(">"),
            tag(">="),
            tag("=="),
            tag("!="),
        )),
        parse_value,
    )))(input)?;
    if let Some((operator, right)) = operator {
        let operator = match operator {
            "+" => Operator::Add,
            "-" => Operator::Subtract,
            "*" => Operator::Multiply,
            "/" => Operator::Divide,
            "<" => Operator::LessThan,
            "<=" => Operator::LessThanOrEqual,
            ">" => Operator::GreaterThan,
            ">=" => Operator::GreaterThanOrEqual,
            "==" => Operator::Equal,
            "!=" => Operator::NotEqual,
            _ => unreachable!(),
        };
        let left = Expression::Num(val);
        let right = Expression::Num(right);
        let expression = match operator {
            Operator::Add
            | Operator::Subtract
            | Operator::Multiply
            | Operator::Divide
            | Operator::LessThan
            | Operator::LessThanOrEqual
            | Operator::GreaterThan
            | Operator::GreaterThanOrEqual
            | Operator::Equal
            | Operator::NotEqual => Expression::Operation {
                left: Box::new(left),
                operator,
                right: Box::new(right),
            },
        };
        Ok((input, expression))
    } else {
        Ok((input, Expression::Num(val)))
    }
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
    let (input, (declaration, _)) = tuple((
        parse_declaration,
        char(';'),
    ))(input)?;
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
        parse_declaration_statement,
        parse_declaration,
        parse_assignment,
        parse_return,
    ))(input)
}

fn parse_statements(input: &str) -> IResult<&str, Vec<Statement>> {
    let (input, statements) = many0(preceded(multispace0, parse_statement))(input)?;
    Ok((input, statements))
}

fn parse_function_declaration(input: &str) -> IResult<&str, Function> {
    let (input, (_, function_type, _, name, _, _, args, _, _, _, _, body, _, _)) = tuple((
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
    fn test_parse_int() {
        let (input, int) = parse_int("10").unwrap();
        assert_eq!(input, "");
        assert_eq!(int, NumType::Int(10));
    }

    #[test]
    fn test_parse_float() {
        let (input, float) = parse_float("10.5").unwrap();
        assert_eq!(input, "");
        assert_eq!(float, NumType::Float(10.5));
    }

    #[test]
    fn test_parse_char() {
        let (input, char) = parse_char("'a'").unwrap();
        assert_eq!(input, "");
        assert_eq!(char, NumType::Char('a'));
    }

    #[test]
    fn test_parse_variable() {
        let (input, variable) = parse_variable("x").unwrap();
        assert_eq!(input, "");
        assert_eq!(variable, NumType::Variable("x".to_string()));
    }

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
        let (input, expression) = parse_expression("x < 10").unwrap();
        assert_eq!(input, "");
        assert_eq!(
            expression,
            Expression::Operation {
                left: Box::new(Expression::Num(NumType::Variable("x".to_string()))),
                operator: Operator::LessThan,
                right: Box::new(Expression::Num(NumType::Int(10))),
            }
        );
        let (input, expression) = parse_expression("x + 10").unwrap();
        assert_eq!(input, "");
        assert_eq!(
            expression,
            Expression::Operation {
                left: Box::new(Expression::Num(NumType::Variable("x".to_string()))),
                operator: Operator::Add,
                right: Box::new(Expression::Num(NumType::Int(10))),
            }
        );
        let (input, expression) = parse_expression("x").unwrap();
        assert_eq!(input, "");
        assert_eq!(
            expression,
            Expression::Num(NumType::Variable("x".to_string()))
        );
    }

    #[test]
    fn test_parse_declaration() {
        let (input, declaration) = parse_declaration("int x").unwrap();
        assert_eq!(input, "");
        assert_eq!(
            declaration,
            Statement::Declaration {
                var_type: "int".to_string(),
                name: "x".to_string(),
                value: None,
            }
        );
        let (input, declaration) = parse_declaration_statement("int x = 10;").unwrap();
        assert_eq!(input, "");
        assert_eq!(
            declaration,
            Statement::Declaration {
                var_type: "int".to_string(),
                name: "x".to_string(),
                value: Some(Expression::Num(NumType::Int(10))),
            }
        );
        let (input, declaration) = parse_declaration_statement("char c = 'a';").unwrap();
        assert_eq!(input, "");
        assert_eq!(
            declaration,
            Statement::Declaration {
                var_type: "char".to_string(),
                name: "c".to_string(),
                value: Some(Expression::Num(NumType::Char('a'))),
            }
        );
    }

    #[test]
    fn test_parse_assignment() {
        let (input, assignment) = parse_assignment("x = x - 1;").unwrap();
        assert_eq!(input, "");
        assert_eq!(
            assignment,
            Statement::Assignment {
                name: "x".to_string(),
                value: Expression::Operation {
                    left: Box::new(Expression::Num(NumType::Variable("x".to_string()))),
                    operator: Operator::Subtract,
                    right: Box::new(Expression::Num(NumType::Int(1))),
                }
            }
        );
    }

    #[test]
    fn test_parse_while() {
        let (input, while_loop) =
            parse_while("while (x) \n{\n\tx = x - 1;\n\tchar c = 'a';\n }").unwrap();
        assert_eq!(input, "");
        assert_eq!(
            while_loop,
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
    }

    #[test]
    fn test_parse_return() {
        let (input, return_statement) = parse_return("return x;").unwrap();
        assert_eq!(input, "");
        assert_eq!(
            return_statement,
            Statement::Return {
                value: Expression::Num(NumType::Variable("x".to_string())),
            }
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
    }

    #[test]
    fn test_parse_statements() {
        let input = r#"
        int x= 10;
        float y;
        y = 10.5;
        while (x) {
            x = x - 1;
            char c = 'a';
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
                            value: Some(Expression::Num(NumType::Char('a'))),
                        }
                    ],
                }
            ]
        );
    }

    #[test]
    fn test_parse_function_declaration() {
        let input = r#"
        int main(int x, int y) {
            int z = x + y;
            return z;
        }"#;

        let (input, function) = parse_function_declaration(input).unwrap();
        assert_eq!(input, "");
        assert_eq!(
            function,
            Function {
                function_type: "int".to_string(),
                name: "main".to_string(),
                args: vec![
                    Statement::Declaration {
                        var_type: "int".to_string(),
                        name: "x".to_string(),
                        value: None,
                    },
                    Statement::Declaration {
                        var_type: "int".to_string(),
                        name: "y".to_string(),
                        value: None,
                    }
                ],
                body: vec![Statement::Declaration {
                    var_type: "int".to_string(),
                    name: "z".to_string(),
                    value: Some(Expression::Operation {
                        left: Box::new(Expression::Num(NumType::Variable("x".to_string()))),
                        operator: Operator::Add,
                        right: Box::new(Expression::Num(NumType::Variable("y".to_string()))),
                    }),
                },
                Statement::Return {
                    value: Expression::Num(NumType::Variable("z".to_string())),
                }],
            }
        );
    }
}
