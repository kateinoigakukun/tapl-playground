import TaPLPlayground

enum Token {
    case `true`, `false`,
    `if`, then, `else`,
    leftParen, rightParen,
    `zero`, succ, pred, iszero,
    eof
}

enum LexPhase: ParserPhase {
    typealias Collection = String
}

typealias Lexer<T> = Parser<LexPhase, T>

let tokenKeywordMap: [String: Token] = [
    "true": .true,
    "false": .false,
    "if": .if,
    "then": .then,
    "else": .else,
    "0": .zero,
    "succ": .succ,
    "pred": .pred,
    "iszero": .iszero,
    "(": .leftParen,
    ")": .rightParen,
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
            + [const(.eof) <^> lexEof()]
    )

}

struct TermInfo {
    let offset: String.Index

    static func dummy() -> TermInfo {
        TermInfo(offset: "evaluator generated info".startIndex)
    }
}

indirect enum Term {
    case `true`(TermInfo), `false`(TermInfo),
    `if`(Term, Term, Term, TermInfo),
    `zero`(TermInfo),
    succ(Term, TermInfo),
    pred(Term, TermInfo),
    iszero(Term, TermInfo)
}

extension Term: CustomStringConvertible {
    var description: String {
        switch self {
        case .true(_): return "true"
        case .false(_): return "false"
        case let .if(t1, t2, t3, _):
            return "if \(t1) then \(t2) else \(t3)"
        case .zero(_): return "0"
        case let .succ(t1, _):
            return "succ(\(t1))"
        case let .pred(t1, _):
            return "pred(\(t1))"
        case let .iszero(t1, _):
            return "iszero(\(t1))"
        }
    }
}

enum TermPhase: ParserPhase {
    typealias Collection = [(Token, String.Index)]
}

typealias TermParser<T> = Parser<TermPhase, T>

func matchToken(_ token: Token) -> TermParser<(Token, String.Index)> {
    satisfy(predicate: { $0.0 == token })
}

func parseTerm1() -> TermParser<Term> {
    Parser { input in
        let (_, offset) = input.cursor
        let info = TermInfo(offset: offset)
        let parser = parseTerm2().map { (constructor) -> Term in
            constructor(info)
        }
        return try parser.parse(input)
    }
}

func parseTerm2() -> TermParser<(TermInfo) -> Term> {
    choice([
        const(curry(Term.true)) <^> matchToken(.true),
        const(curry(Term.false)) <^> matchToken(.false),
        (
            curry(Term.if) <^>
                (matchToken(.if) *> parseTerm1()) <*>
                (matchToken(.then) *> parseTerm1()) <*>
                (matchToken(.else) *> parseTerm1())
        ),
        const(Term.zero) <^> matchToken(.zero),
        curry(Term.succ) <^> (matchToken(.succ) *>
            matchToken(.leftParen) *>
            parseTerm1()
            <* matchToken(.rightParen)),
        curry(Term.pred) <^> (matchToken(.pred) *>
            matchToken(.leftParen) *>
            parseTerm1()
            <* matchToken(.rightParen)),
        curry(Term.iszero) <^> (matchToken(.iszero) *>
            matchToken(.leftParen) *>
            parseTerm1()
            <* matchToken(.rightParen)),
    ])
}

func parse(_ tokens: [(Token, String.Index)]) throws -> Term {
    try parseTerm1().parse(.root(tokens)).0
}

enum Value {
    case `true`, `false`, number(Number)
}

indirect enum Number {
    case zero, succ(Number)
}

func isNumericValue(_ term: Term) -> Bool {
    switch term {
    case .zero(_): return true
    case .succ(let t1, _): return isNumericValue(t1)
    default: return false
    }
}

func isValue(_ term: Term) -> Bool {
    switch term {
    case .true(_), .false(_): return true
    case _ where isNumericValue(term): return true
    default:
        return false
    }
}

enum EvalError: Swift.Error {
    case noRuleApplies
}

func eval1(_ term: Term) throws -> Term {
    switch term {
    case let .if(.true(_), t2, _, _):
        return t2
    case let .if(.false(_), _, t3, _):
        return t3
    case let .if(t1, t2, t3, info):
        return try .if(eval1(t1), t2, t3, info)
    case let .succ(t1, info):
        return try .succ(eval1(t1), info)
    case .pred(.zero(_), _):
        return .zero(.dummy())
    case let .pred(.succ(nv1, _), _) where isNumericValue(nv1):
        return nv1
    case let .pred(t1, info):
        return try .pred(eval1(t1), info)
    case .iszero(.zero(_), _):
        return .true(.dummy())
    case let .iszero(.succ(nv1, _), _) where isNumericValue(nv1):
        return .false(.dummy())
    case let .iszero(t1, info):
        return try .iszero(eval1(t1), info)
    default:
        throw EvalError.noRuleApplies
    }
}

func eval(_ term: Term) -> Term {
    do {
        let t = try eval1(term)
        return eval(t)
    } catch {
        return term
    }
}
