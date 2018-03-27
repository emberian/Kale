
struct Context {
    names: VecMap<String>,

}
struct Ix<T>(Box<T>);

enum Type {
    Star,
    Arrow(Ix<Kind>, Ix<Kind>),
}

struct Name<T>(u64);

type Spine<T> = Vec<T>;

struct E;
struct V;
struct P;

enum Syntax<M> {
    // These can be values, expressions, or patterns!
    Par(Ix<Syntax<M>>, Ix<Syntax<M>>),
    Inl(Ix<Syntax<M>>),
    Inr(Ix<Syntax<M>>),

    // Values or expressions
    Var(Name<E>),
    Unt,

    // Expressions
    Abs(Name<E>, Ix<Syntax<E>>),
    App(Name<E>, Spine<Syntax<M>>), // generalize maybe?
    Ann(Ix<Syntax<M>>, Ix<Type>),
    Cse(Ix<Syntax<E>>, Vec<(Ix<Syntax<P>>, Ix<Syntax<E>>)>)
}

type Expr = Syntax<Expr>;
type Val = Syntax<Val>;
type Pat = Syntax<Pat>;

enum Sort {
    Star,
    Etar(u64),
}

struct FT;
struct MT;

struct Prop<T> = Ix<(T, T)>;

enum Polarity {
    Pos, Neg
}

struct U;
struct EX;

enum GVar {
    Uni(Name<U>),
    Exi(Name<EX>),
}

// This is probably going to bite me solidly in the ass
enum Type<M> {
    // Either monotypes or full types
    One,
    Var(GVar),
    Arr(Ix<Type<M>>, Ix<Type<M>>),
    Sum(Ix<Type<M>>, Ix<Type<M>>),
    Prd(Ix<Type<M>>, Ix<Type<M>>),

    // Only monotypes
    Zero,
    Succ(Ix<Type<MT>>),

    // Only full types
    All(Name, Sort, Ix<Type<FT>>),
    Exi(Name, Sort, Ix<Type<FT>>),
    Imp(Prop<Type<MT>>, Ix<Type<FT>>),
    Wth(Prop<Type<MT>>, Ix<Type<FT>),
}
enum CtxEnt {
    TypeDecl(Name<U>, Sort),
    VarDecl(Name<E>, Type<FT>, Polarity),

    Unsolved(Name<EX>, Sort),
    Solved(Name<EX>, Sort, Type<MT>),
    Equation(Name<U>, Type<MT>),

    Marker(u64),
}

type Context = Vec<CtxEnt>;

struct Tcx {
    real_context: Context,
}

impl TCx {
    fn push_ctx(&mut self, e: CtxEnt);
    fn pop_ctx(&mut self) -> CtxEnt;
    fn peek_ctx(&self) -> &CtxEnt;
    fn ctx_has_eqn(&self, n: Name<U>) -> Option(Ix<Type<FT>>) {
        for ent in self.real_context.iter().rev() {
            match ent {
                CtxEnt::Equation(n_, t) if n == n_ => return Some(t.clone()),
                _ => continue
            }
        }
        None
    }
    fn ctx_has_sln(&self, n: Name<EX>) -> Option(Ix<Type<FT>>, impl Iterator<Item=&CtxEnt>) {
        for ent in self.real_context.iter().rev() {
            match ent {
                CtxEnt::Solved(n_, _, t) if n == n_ => return Some(t.clone()),
                _ => continue
            }
        }
        None
    }

    fn apply_ctx(&self, to: Ix<Type<FT>>) -> Type<FT> {
        match *to {
            &Type::One => Type::One,
            &Type::Var(GVar::Uni(n)) => match self.ctx_has_eqn(n) {
                Some(t) => self.apply_ctx(&*t),
                None => to.clone()
            },
            &Type::Var(GVar::Exi(n)) => match self.ctx_has_sln(n) {
                Some(t) => 
            }
        }
    }
    fn instantiate(&mut self, n: Name<EX>, )
    fn check_equation(&mut self, t1: Type<MT>, t2: Type<MT>, s: Sort) -> Ctx;

}
