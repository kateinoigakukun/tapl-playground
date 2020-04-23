import TaPLPlayground

enum Token: Equatable {
    case identifier(String)
    case backslash
    case dot
    case colon
    case arrow
    case leftParen, rightParen
    case `true`, `false`
    case eof
}

enum LexPhase: ParserPhase {
    typealias Collection = String
}

typealias Lexer<T> = Parser<LexPhase, T>

let tokenKeywordMap: [String: Token] = [
    "\\": .backslash,
    ".": .dot,
    ":": .colon,
    "->": .arrow,
    "(": .leftParen,
    ")": .rightParen,
    "true": .true,
    "false": .false,
]

func lex(_ text: String) throws -> [(Token, String.Index)] {
    try lex().parse(.root(text)).0
}

func lex() -> Lexer<[(Token, String.Index)]> {
    withOffset(lexToken()).flatMap { (token, offset) in
        switch token {
        case .eof: return .pure([(.eof, offset)])
        default:
            return cons((token, offset)) <^> (skipSpaces() *> lex())
        }
    }
}

func lexToken() -> Lexer<Token> {
    choice(
        tokenKeywordMap.map { const($1) <^> token($0) }
            + [
                Token.identifier <^> stringUntil(["\\", ".", ":", " ", "(", ")", "-"]),
                const(.eof) <^> lexEof()
        ]
    )
}

enum TermPhase: ParserPhase {
    typealias Collection = [(Token, String.Index)]
}

typealias TermParser<T> = Parser<TermPhase, T>

struct TermInfo {
    let offset: String.Index

    static func dummy() -> TermInfo {
        TermInfo(offset: "evaluator generated info".startIndex)
    }
}

indirect enum Term {
    case variable(String, TermInfo)
    case lambda(parameter: String, Type, body: Term, TermInfo)
    case apply(Term, Term, TermInfo)
    case `true`(TermInfo), `false`(TermInfo)
}

func parse(_ tokens: [(Token, String.Index)]) throws -> Term {
    try wrapParser(parseTerm2()).parse(.root(tokens)).0
}

func wrapParser(_ subParser: @escaping @autoclosure () -> TermParser<(TermInfo) -> Term>) -> TermParser<Term> {
    Parser { input in
        let (_, offset) = input.cursor
        let info = TermInfo(offset: offset)
        let parser = subParser().map { (constructor) -> Term in
            constructor(info)
        }
        return try parser.parse(input)
    }
}

func consumeIdentifier() -> TermParser<String> {
    consumeMap { tk, _ in
        guard case let .identifier(id) = tk else { return nil }
        return id
    }
}

let primitiveTypeMap: [String: Type] = [
    "Bool": .bool
]

func parseType() -> TermParser<Type> {
    choice(
        primitiveTypeMap.map { pair in
            const(pair.value) <^>
                satisfy { (tk, _) in
                    Token.identifier(pair.key) == tk
                }
            }
        +
        [
            curry(Type.arrow)
                <^> parseType()
                <*> (
                    satisfy(predicate: { $0.0 == .arrow }) *> parseType()
                )
        ]
    )
}


func parseTerm2() -> TermParser<(TermInfo) -> Term> {
    choice([
        (curry(Term.lambda)
            <^> (
                satisfy { $0.0 == .backslash } *> consumeIdentifier()
            )
            <*> (satisfy { $0.0 == .colon } *> parseType())
            <*> (
                satisfy { $0.0 == .dot } *> wrapParser(parseTerm2())
            )
        ),
        (
            satisfy { $0.0 == .leftParen } *> (
                curry(Term.apply)
                    <^> wrapParser(parseTerm2())
                    <*> wrapParser(parseTerm2())
            ) <*
                satisfy { $0.0 == .rightParen }
        ),
        (curry(Term.variable) <^> consumeIdentifier()),
        (const(Term.true) <^> satisfy { $0.0 == .true }),
        (const(Term.false) <^> satisfy { $0.0 == .false }),
    ])
}

indirect enum NoNameTerm {
    case variable(Int, ctxLength: Int, TermInfo)
    case lambda(nameHint: String, ty: Type, body: NoNameTerm, TermInfo)
    case apply(NoNameTerm, NoNameTerm, TermInfo)
    case `true`(TermInfo), `false`(TermInfo)

    func dump() -> String {
        switch self {
        case let .variable(idx, _, _):
            return "\(idx)"
        case let .lambda(_, _, body, _):
            return "\\. \(body.dump())"
        case let .apply(lambda, input, _):
            return "(\(lambda.dump()) \(input.dump()))"
        case .true(_): return "true"
        case .false(_): return "false"
        }
    }
}


indirect enum Type: Equatable {
    case arrow(Type, Type)
    case bool
}

enum Binding {
    case nameBind
    case varBind(Type)
}

struct NameEnvironment {
    let values: [(name: String, bind: Binding)]

    var count: Int { values.count }
    func getIndex(for name: String) -> Int {
        values.count - values.lastIndex(where: { $0.name == name })! - 1
    }
    func with(_ name: String, bind: Binding) -> NameEnvironment {
        return .init(values: values + [(name, bind)])
    }
    
    func getType(_ index: Int) -> Type {
        switch values[index].bind {
        case .varBind(let ty): return ty
        default: fatalError()
        }
    }
}

func removeNames(_ term: Term) -> NoNameTerm {
    removeNames(term, env: NameEnvironment(values: []))
}
func removeNames(_ term: Term, env: NameEnvironment) -> NoNameTerm {
    switch term {
    case let .lambda(parameter, ty, body, info):
        return .lambda(
            nameHint: parameter,
            ty: ty,
            body: removeNames(
                body,
                env: env.with(parameter, bind: .nameBind)
            ),
            info
        )
    case let .variable(name, info):
        return .variable(env.getIndex(for: name), ctxLength: env.count, info)
    case let .apply(lambda, input, info):
        return .apply(
            removeNames(lambda, env: env),
            removeNames(input, env: env),
            info
        )
    case let .true(info):
        return .true(info)
    case let .false(info):
        return .false(info)
    }
}

enum TypeError: Error {
    case typeMismatch(Type, Type)
    case typeShouldArrow(Type)
}

func typeof(_ term: NoNameTerm, env: NameEnvironment) throws -> Type {
    switch term {
    case let .variable(x, _, _): return env.getType(x)
    case let .lambda(param, ty, body, _):
        let ctx = env.with(param, bind: .varBind(ty))
        let r = try typeof(body, env: ctx)
        return .arrow(ty, r)
    case let .apply(t1, t2, _):
        let tyT1 = try typeof(t1, env: env)
        let tyT2 = try typeof(t2, env: env)
        guard case let .arrow(input, output) = tyT1 else {
            throw TypeError.typeShouldArrow(tyT1)
        }
        guard input == tyT2 else {
            throw TypeError.typeMismatch(input, tyT2)
        }
        return output
    case .true(_), .false(_):
        return .bool
    }
}

func typeof(_ term: NoNameTerm) throws -> Type {
    try typeof(term, env: NameEnvironment(values: []))
}
