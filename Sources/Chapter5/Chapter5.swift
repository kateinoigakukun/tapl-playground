import TaPLPlayground

enum Token: Equatable {
    case identifier(String)
    case backslash
    case dot
    case leftParen, rightParen
    case eof
}

enum LexPhase: ParserPhase {
    typealias Collection = String
}

typealias Lexer<T> = Parser<LexPhase, T>

let tokenKeywordMap: [String: Token] = [
    "\\": .backslash,
    ".": .dot,
    "(": .leftParen,
    ")": .rightParen
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
                Token.identifier <^> stringUntil(["\\", ".", " ", "(", ")"]),
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
    case lambda(parameter: String, body: Term, TermInfo)
    case apply(Term, Term, TermInfo)
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

func parseTerm2() -> TermParser<(TermInfo) -> Term> {
    choice([
        (curry(Term.lambda)
            <^> (
                satisfy {
                    guard case .backslash = $0.0 else { return false }
                    return true
                } *> consumeIdentifier()
            )
            <*> (
                satisfy {
                    guard case .dot = $0.0 else { return false }
                    return true
                } *> wrapParser(parseTerm2())
            )
        ),
        (
            satisfy {
                guard case .leftParen = $0.0 else { return false }
                return true
            } *> (
                curry(Term.apply)
                    <^> wrapParser(parseTerm2())
                    <*> wrapParser(parseTerm2())
            ) <*
            satisfy {
                guard case .rightParen = $0.0 else { return false }
                return true
            }
        ),
        (curry(Term.variable) <^> consumeIdentifier())
    ])
}

indirect enum NoNameTerm {
    case variable(Int, ctxLength: Int, TermInfo)
    case lambda(nameHint: String, body: NoNameTerm, TermInfo)
    case apply(NoNameTerm, NoNameTerm, TermInfo)

    func dump() -> String {
        switch self {
        case let .variable(idx, _, _):
            return "\(idx)"
        case let .lambda(_, body, _):
            return "\\. \(body.dump())"
        case let .apply(lambda, input, _):
            return "(\(lambda.dump()) \(input.dump()))"
        }
    }
}

struct NameEnvironment {
    let names: [String]

    var count: Int { names.count }
    func getIndex(for name: String) -> Int {
        names.count - names.lastIndex(where: { $0 == name })! - 1
    }
    func withName(_ name: String) -> NameEnvironment {
        return .init(names: names + [name])
    }
}

func removeNames(_ term: Term) -> NoNameTerm {
    removeNames(term, env: NameEnvironment(names: []))
}
func removeNames(_ term: Term, env: NameEnvironment) -> NoNameTerm {
    switch term {
    case let .lambda(parameter, body, info):
        return .lambda(
            nameHint: parameter,
            body: removeNames(
                body,
                env: env.withName(parameter)
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
    }
}

typealias Context = [String]

func pickFreshName(hint: String, context: Context) -> (Context, String) {
    if context.contains(hint) {
        return pickFreshName(hint: "\(hint)'", context: context)
    } else {
        return (context + [hint], hint)
    }
}

func printTerm(_ term: NoNameTerm, context: Context) -> String {
    switch term {
    case let .lambda(x, t1, _):
        let (ctx, name) = pickFreshName(hint: x, context: context)
        return "(lambda \(name). \(printTerm(t1, context: ctx)))"
    case let .apply(t1, t2, _):
        return "(\(printTerm(t1, context: context)) \(printTerm(t2, context: context)))"
    case let .variable(x, n, _):
        if context.count == n {
            return context[context.count - x - 1]
        } else {
            return "[bad index]"
        }
    }
}

func printTerm(_ term: NoNameTerm) -> String {
    printTerm(term, context: [])
}

func termShift(diff: Int, term: NoNameTerm) -> NoNameTerm {
    func walk(absDepth: Int, term: NoNameTerm) -> NoNameTerm {
        switch term {
        case let .variable(x, n, info):
            if x >= absDepth {
                return .variable(x + diff, ctxLength: n + diff, info)
            } else {
                return .variable(x, ctxLength: n + diff, info)
            }
        case let .lambda(nameHint, body, info):
            return .lambda(
                nameHint: nameHint,
                body: walk(absDepth: absDepth + 1, term: body),
                info
            )
        case let .apply(t1, t2, info):
            return .apply(
                walk(absDepth: absDepth, term: t1),
                walk(absDepth: absDepth, term: t2),
                info
            )
        }
    }
    return walk(absDepth: 0, term: term)
}

// [j -> s] t
func termSubst(_ j: Int, _ substitution: NoNameTerm, _ rootTarget: NoNameTerm) -> NoNameTerm {
    func walk(_ nestCount: Int, _ target: NoNameTerm) -> NoNameTerm {
        switch target {
        case let .variable(x, n, info):
            if x == j + nestCount {
                // Shift only when substitute
                return termShift(diff: nestCount, term: substitution)
            } else {
                return .variable(x, ctxLength: n, info)
            }
        case let .lambda(nameHint, body, info):
            return .lambda(
                nameHint: nameHint,
                body: walk(nestCount + 1, body),
                info
            )
        case let .apply(t1, t2, info):
            return .apply(walk(nestCount, t1), walk(nestCount, t2), info)
        }
    }
    return walk(0, rootTarget)
}

func termSubstTop(_ s: NoNameTerm, _ t0: NoNameTerm) -> NoNameTerm {
    // Shift +1 to substitute it in the term
    let subst = termShift(diff: 1, term: s)
    let t1 = termSubst(0, subst, t0)
    // Shift -1 to indicate the outer binding has gone
    return termShift(diff: -1, term: t1)
}

func isValue(_ term: NoNameTerm) -> Bool {
    switch term {
    case .lambda: return true
    default: return false
    }
}

enum EvalError: Swift.Error {
    case noRuleApplies
}

func eval1(_ term: NoNameTerm) throws -> NoNameTerm {
    switch term {
    case let .apply(.lambda(_, body, _), v2, _) where isValue(v2):
        return termSubstTop(v2, body)
    case let .apply(v1, t2, info) where isValue(v1):
        return try .apply(v1, eval1(t2), info)
    case let .apply(t1, t2, info):
        return try .apply(eval1(t1), t2, info)
    default: throw EvalError.noRuleApplies
    }
}

func eval(_ term: NoNameTerm) -> NoNameTerm {
    do {
        let t = try eval1(term)
        return eval(t)
    } catch {
        return term
    }
}
