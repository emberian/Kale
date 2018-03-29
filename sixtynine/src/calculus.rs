
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

type IType<T> = Ix<Type<T>>;

struct Context {
    list: Vec<CtxEnt>,
}
impl Context {
    fn with(&self, e: CtxEnt) -> Context {
        // lol
        let mut new = self.list.clone();
        new.push(e);
        Context {
            list: new
        }
    }
    fn without(&self, idx: usize) -> Context {
        // also lol but not as much
        let mut new = self.list.clone();
        new.remove(new.len() - idx + 1);
        Context {
            list: new
        }
    }
    fn push(&mut self, e: CtxEnt) {
        self.list.push(e);
    }
    fn pop(&mut self) -> Option<CtxEnt> {
        self.list.pop()
    }
    fn peek(&self) -> Option<&CtxEnt> {
        self.list.last()
    }
    fn peek_mut(&mut self) -> Option<&mut CtxEnt> {
        self.list.last_mut()
    }
    fn iter(&self) -> impl Iterator<Item=&CtxEnt> {
        self.list.iter().rev()
    }
    fn iter_mut(&mut self) -> impl Iterator<Item=&mut CtxEnt> {
        self.list.iter_mut().rev()
    }
    fn find_equation(&self, n: Name<U>) -> Option<IType<MT>> {
        for itm in self.iter() {
            match itm {
                &CtxEnt::Equation(n2, ty) if n == n2 => return Some(ty.clone()),
                _ => continue
            }
        }
        None
    }
    fn find_solution(&self, n: Name<EX>) -> Option<(usize, IType<MT>)> {
        for (idx, itm) in self.iter().enumerate() {
            match itm {
                &CtxEnt::Solved(n2, _, ty) if n == n2 => return Some((idx, ty.clone())),
                _ => continue
            }
        }
        None
    }

    fn apply_complete(&self, omega: &Context) -> Context {
        let mut new_ctx = Context { list: vec![] };
        for (omega, gamma) in omega.iter().zip(self.iter()) { match (omega, gamma) {
            
        } }
    }
}

struct Tcx {
    ctx: Context,
}

impl TCx {
    fn push_ctx(&self, e: CtxEnt) -> TCx;
    fn pop_ctx(&self) -> TCx;
    fn peek_ctx(&self) -> &CtxEnt;

    fn apply_ctx<T>(&self, to: IType<T>) -> IType<T> {
        match *to {
            Type::One => to.clone(),
            Type::Var(GVar::Uni(n)) => match self.ctx.find_equation(n) {
                Some(t) => self.apply_ctx(t),
                None => to.clone()
            },
            Type::Var(GVar::Exi(n)) => match self.ctx.find_solution(n) {
                Some((idx, t)) => Tcx { 
                    ctx: self.ctx.without(idx)
                }.apply_ctx(t),
                None => to.clone()
            },
            Type::Arr(ref a, ref b) => Type::Arr(self.apply_ctx(a), self.apply_ctx(b)).mk(),
            Type::Sum(ref a, ref b) => Type::Sum(self.apply_ctx(a), self.apply_ctx(b)).mk(),
            Type::Prd(ref a, ref b) => Type::Prd(self.apply_ctx(a), self.apply_ctx(b)).mk(),
            Type::Imp(ref a, ref b) => Type::Imp(self.apply_ctx(a), self.apply_ctx(b)).mk(),
            Type::Wth(ref a, ref b) => Type::Wth(self.apply_ctx(a), self.apply_ctx(b)).mk(),

            Type::All(n, s, ref t) => Type::All(n, s, self.apply_ctx(t)).mk(),
            Type::Exi(n, s, ref t) => Type::Exi(n, s, self.apply_ctx(t)).mk(),
            Zero => to.clone(),
            Succ(ref t) => to.clone(),
        }
    }
    fn instantiate(&mut self, n: Name<EX>, )
    fn check_equation(&mut self, t1: Type<MT>, t2: Type<MT>, s: Sort) -> Ctx;
}
