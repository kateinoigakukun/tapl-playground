import TaPLPlayground

enum Token {
}

enum LexPhase: ParserPhase {
    typealias Collection = String
}

typealias Lexer<T> = Parser<LexPhase, T>

let tokenKeywordMap: [String: Token] = [

]
