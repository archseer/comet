use crate::error::Error;
use comet_parser as parser;
use parser::ast::{Expr, Ident};
use parser::diagnostics::{ByteIndex, Span};
use parser::symbol::Symbol;
// use std::cell::RefCell;

#[derive(Clone, PartialEq)]
pub enum Type {
    // type constant: `int` or `bool`
    // Const (but we can just use a predefined constructor)
    /// Type application: `List<int>`
    App {
        name: Box<Type>,
        args: Vec<Type>,
    },
    /// Function type: `(int, int) -> int`
    Fn {
        args: Vec<Type>,
        ret: Box<Type>,
    },
    /// Type variable
    Var(Var),
    Const(Const),
    // Tuple
    // Map / Record
}

impl std::fmt::Debug for Type {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        use itertools::Itertools;

        match &self {
            Type::Fn { args, ret } => {
                write!(fmt, "(")?;
                let line: Vec<_> = args
                    .iter()
                    .map(|a| format!("{:?}", a))
                    .intersperse(", ".to_owned())
                    .collect();
                for item in line {
                    write!(fmt, "{}", item)?;
                }
                write!(fmt, ") -> {:?}", ret)
            }
            Type::App { name, args } => {
                write!(fmt, "{:?}<", name)?;
                let line: Vec<_> = args
                    .iter()
                    .map(|a| format!("{:?}", a))
                    .intersperse(", ".to_owned())
                    .collect();
                for item in line {
                    write!(fmt, "{}", item)?;
                }
                write!(fmt, ">")
            }
            Type::Var(Var::Link(t)) => write!(fmt, "{:?}", t),
            Type::Var(Var::Generic(id)) => write!(fmt, "{:?}", id),
            Type::Var(Var::Unbound { id, .. }) => write!(fmt, "{:?}", id),
            Type::Const(Const::Int) => write!(fmt, "Int"),
            Type::Const(Const::Bool) => write!(fmt, "Bool"),
            Type::Const(Const::Unit) => write!(fmt, "()"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Const {
    Int,
    Bool,
    Unit,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Var {
    Unbound { id: usize, level: usize },
    Link(Box<Type>),
    Generic(usize),
}

pub struct Env {
    id: usize,

    // TODO: use symbols
    variables: im::HashMap<Symbol, Type>,
    // modules: HashMap<Symbol, ()>
    // type_constructors
}

impl Env {
    pub fn new() -> Self {
        Env {
            id: 0,
            variables: im::HashMap::new(),
        }
    }

    fn next_id(&mut self) -> usize {
        let i = self.id;
        self.id += 1;
        i
    }

    fn new_var(&mut self, level: usize) -> Type {
        Type::Var(Var::Unbound {
            id: self.next_id(),
            level,
        })
    }

    fn new_generic_var(&mut self) -> Type {
        Type::Var(Var::Generic(self.next_id()))
    }
}

fn adjust_levels(ty: &mut Type, id: usize, level: usize) -> Result<(), Error> {
    match ty {
        Type::Var(Var::Link(ty)) => adjust_levels(ty, id, level),
        Type::Var(Var::Generic(..)) => panic!("generic types cannot be processed by adjust_levels"),
        Type::Var(Var::Unbound {
            id: other_id,
            level: other_level,
        }) => {
            if id == *other_id {
                Err(Error::RecursiveType)
            } else if *other_level > level {
                *other_level = level;
                Ok(())
            } else {
                Ok(())
            }
        }
        Type::App { name, args } => {
            adjust_levels(name, level, id)?;

            for arg in args.iter_mut() {
                adjust_levels(arg, level, id)?;
            }
            Ok(())
        }
        Type::Fn { args, ret } => {
            for arg in args.iter_mut() {
                adjust_levels(arg, level, id)?;
            }
            adjust_levels(ret, level, id)
        }
        Type::Const(_) => Ok(()),
    }
}

/// Takes two types and tries to unify them, i.e. determine if they can be equal.
fn unify<'a>(t1: &'a mut Type, t2: &'a mut Type, env: &mut Env) -> Result<(), Error> {
    if t1 == t2 {
        return Ok(());
    }

    match (t1, t2) {
        (Type::Const(name1), Type::Const(name2)) if name1 == name2 => Ok(()),
        (Type::App { .. }, Type::App { .. }) => unimplemented!(),
        (
            Type::Fn {
                args: args1,
                ret: ret1,
            },
            Type::Fn {
                args: args2,
                ret: ret2,
            },
        ) => {
            // TODO verify args1 and args2 length
            for (a, b) in args1.iter_mut().zip(args2) {
                unify(a, b, env)?;
            }
            unify(ret1, ret2, env)
        }
        (Type::Var(Var::Link(t1)), ref mut t2) | (ref mut t2, Type::Var(Var::Link(t1))) => {
            unify(t1, t2, env)
        }
        (Type::Var(Var::Unbound { .. }), Type::Var(Var::Unbound { .. })) => {
            panic!("Can't unify two unbound types!")
        }
        (Type::Var(tvar @ Var::Unbound { .. }), ref mut ty)
        | (ref mut ty, Type::Var(tvar @ Var::Unbound { .. })) => {
            if let &mut Var::Unbound { id, level } = tvar {
                adjust_levels(ty, id, level)?;
            };
            *tvar = Var::Link(Box::new(ty.clone()));
            Ok(())
        }
        (t1, t2) => Err(Error::CouldNotUnify {
            t1: t1.clone(),
            t2: t2.clone(),
        }),
    }
}

/// Takes a level and a type and turns all type variables within the type that have level higher than the input level into generalized (polymorphic) type variables.
fn generalize(t: Type, level: usize) -> Type {
    match t {
        Type::Var(Var::Unbound {
            id,
            level: other_level,
        }) if other_level > level => Type::Var(Var::Generic(id)),
        Type::App { name, args } => Type::App {
            name: Box::new(generalize(*name, level)),
            args: args.into_iter().map(|t| generalize(t, level)).collect(),
        },
        Type::Fn { args, ret } => Type::Fn {
            ret: Box::new(generalize(*ret, level)),
            args: args.into_iter().map(|t| generalize(t, level)).collect(),
        },
        Type::Var(Var::Link(t)) => generalize(*t, level),
        Type::Var(Var::Generic(..)) | Type::Var(Var::Unbound { .. }) | Type::Const(..) => t,
    }
}

/// Duplicates the input type, transforming any polymorphic variables into normal unbound type
/// variables.
fn instantiate(t: Type, level: usize, env: &mut Env) -> Type {
    use std::collections::HashMap;
    let mut map = HashMap::new();

    fn f(t: Type, level: usize, ids: &mut HashMap<usize, Type>, env: &mut Env) -> Type {
        match t {
            Type::Const(..) => t,
            Type::Var(Var::Link(t)) => f(*t, level, ids, env),
            Type::Var(Var::Generic(id)) => match ids.get(&id) {
                Some(t) => t.clone(),
                None => {
                    let v = env.new_var(level);
                    ids.insert(id, v.clone());
                    v
                }
            },
            Type::Var(Var::Unbound { .. }) => t,
            Type::App { name, args } => Type::App {
                name: Box::new(instantiate(*name, level, env)),
                args: args.into_iter().map(|t| f(t, level, ids, env)).collect(),
            },
            Type::Fn { args, ret } => Type::Fn {
                ret: Box::new(instantiate(*ret, level, env)),
                args: args.into_iter().map(|t| f(t, level, ids, env)).collect(),
            },
        }
    }

    f(t, level, &mut map, env)
}

fn match_fun_type(t: &mut Type, arity: usize, env: &mut Env) -> Result<(Vec<Type>, Type), Error> {
    match t {
        Type::Fn { args, ret } => {
            if args.len() == arity {
                Ok((args.clone(), *ret.clone()))
            } else {
                Err(Error::UnexpectedArity {
                    expected: arity,
                    given: args.len(),
                })
            }
        }
        Type::Var(Var::Link(t)) => match_fun_type(t, arity, env),
        Type::Var(tvar @ Var::Unbound { .. }) => {
            let (args, ret) = if let &mut Var::Unbound { level, .. } = tvar {
                let args: Vec<_> = (0..arity).map(|_| env.new_var(level)).collect();
                let ret = env.new_var(level);
                (args, ret)
            } else {
                unreachable!()
            };

            *tvar = Var::Link(Box::new(Type::Fn {
                args: args.clone(),
                ret: Box::new(ret.clone()),
            }));
            Ok((args, ret))
        }
        _ => Err(Error::NotFn),
    }
}

/// Takes an environment, a level used for let-generalization, and an expression, and infers types for each term type.
pub fn infer(t: &Expr, env: &mut Env, level: usize) -> Result<Type, Error> {
    match t {
        Expr::Int(..) => Ok(Type::Const(Const::Int)),
        Expr::Bool(..) => Ok(Type::Const(Const::Bool)),
        Expr::Unit => Ok(Type::Const(Const::Unit)),
        Expr::Op(arg1, op, arg2) => {
            // transform binop to call with two args, infer that

            // TODO: do this as desugaring in advance?
            infer(
                &Expr::Call {
                    name: Ident(Symbol::intern(op.to_string())),
                    args: vec![arg1.as_ref().clone(), arg2.as_ref().clone()],
                },
                env,
                level,
            )
        }
        Expr::Var(name) => match env.variables.get(&name.0) {
            Some(t) => Ok(instantiate(t.clone(), level, env)),
            None => Err(Error::UnknownVariable {
                span: Span::new(ByteIndex(0), ByteIndex(0)),
                name: name.clone(),
            }),
        },
        Expr::Lambda { args, body } => {
            // checkpoint the variable env
            let prev_env = env.variables.clone();

            let arg_types: Vec<_> = args.iter().map(|_| env.new_var(level)).collect();

            // generate a new environment with formal parameters
            args.iter().zip(arg_types.iter()).for_each(|(name, t)| {
                env.variables.insert(name.0, t.clone());
            });

            // infer the return type
            let ret = infer_block(body, level, env)?;

            // restore the env
            env.variables = prev_env;

            Ok(Type::Fn {
                args: arg_types,
                ret: Box::new(ret),
            })
        }
        Expr::Let { name, ty, value } => {
            // Resolve the initialization expression
            let initial = infer(value, env, level + 1)?;
            let initial = generalize(initial, level + 1);

            // Type annotation on variable
            match ty {
                None => {
                    env.variables.insert(name.0, initial.clone());
                    Ok(initial)
                }
                Some(ident) => {
                    let ascription = env
                        .variables
                        .get(&ident.0)
                        .expect("TODO: handle ascription error");

                    // check if ascription is a subtype of initial
                    if initial.eq(ascription) {
                        // unify(initial, ascription)
                        Ok(initial)
                    } else {
                        unimplemented!()
                    }
                }
            }
        }
        // TODO: increase level in blocks/scopes
        Expr::Call { name, args } => {
            // lookup the matching fun
            let (mut args_types, ret) = env
                .variables
                .get_mut(&name.0)
                .cloned() // TODO: I don't think this is correct
                .ok_or_else(|| Error::UndefinedFn {
                    span: Span::new(ByteIndex(0), ByteIndex(0)),
                    name: name.clone(),
                })
                .and_then(|mut t| match_fun_type(&mut t, args.len(), env))?;

            // unify arguments
            let args: Vec<Type> = args_types
                .iter_mut()
                .zip(args)
                .map(|(t, arg): (&mut Type, _)| {
                    let mut arg = infer(arg, env, level)?;
                    unify(t, &mut arg, env)?; // TODO: maybe ok if we just return unified, then we don't need to pass mut
                    Ok(arg)
                })
                .collect::<Result<_, _>>()?;

            Ok(ret)
        }
        Expr::If(..) => unimplemented!(),
        Expr::Error => unreachable!(),
    }
}

pub fn infer_fn() {
    // add vars into env
    // infer_block
}

pub fn infer_block(block: &[Expr], level: usize, env: &mut Env) -> Result<Type, Error> {
    let level = level + 1;
    block
        .iter()
        .try_fold(Type::Const(Const::Unit), |_acc, expr| {
            infer(expr, env, level)
        })
}

#[test]
fn infer_test() {
    struct Case {
        input: &'static str,
        result: &'static str,
    };

    let cases = [
        Case {
            result: "Int",
            input: "1",
        },
        Case {
            result: "Bool",
            input: "true",
        },
        Case {
            result: "(0) -> Int",
            input: "fn (a) {
                let b = fn (c) { 2 }
                b(2)
            }",
        },
        // Case {
        //     result: "(0) -> Int",
        //     // TODO: this shouldn't fail since arg unused
        //     input: "fn (a) {
        //         let b = fn (c) { 2 }
        //         b(a)
        //     }",
        // },
        Case {
            result: "(0) -> 0",
            input: "fn (a) {
                a
            }",
        },
        Case {
            result: "(0) -> Int",
            input: "fn (b) {
                let a = 1
                a
            }",
        },
        Case {
            result: "Int",
            input: "fn (a, b) {
                a + b
            }",
        },
    ];

    for Case { input, result } in cases.into_iter() {
        let ast = parser::parser::parse(input).expect("syntax error");

        let mut env = Env::new();

        let t = infer(&ast, &mut env, 1).expect("unable to infer type");
        assert_eq!((input, format!("{:?}", t).as_str()), (input, *result));
    }
}

#[test]
fn infer_error_case() {
    struct Case {
        input: &'static str,
        // error: Error,
    };

    let cases = [Case {
        input: "a",
        // result: Error::UnknownVariable {}
    }];

    for Case { input } in cases.into_iter() {
        let ast = parser::parser::parse(input).expect("syntax error");

        let mut env = Env::new();

        let t = infer(&ast, &mut env, 1);
        assert!(t.is_err());
    }
}
