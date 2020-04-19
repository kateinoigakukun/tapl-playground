import XCTest
@testable import Chapter3

final class Chapter3Tests: XCTestCase {
    func testParse() throws {
        let inputs: [String: (Term, String) -> Void] = [
            "if iszero(0) then true else false": { term, input in
                switch term {
                case .if(.iszero(.zero, _), .true, .false, _): return
                default: XCTFail()
                }
            },
            "succ(succ(succ(0)))": { term, input in
                switch term {
                case .succ(.succ(.succ(.zero, _), _), _): return
                default: XCTFail()
                }
            },
            "succ(if true then true else false)": { term, input in
                switch term {
                case .succ(.if(.true, .true, .false, _), _): return
                default: XCTFail()
                }
            },
        ]
        for (input, expect) in inputs {
            let tokens = try lex(input)
            let syntax = try parse(tokens)
            expect(syntax, input)
        }
    }

    func testEval() throws {
        let inputs: [String: (Term, String) -> Void] = [
            "if iszero(0) then true else false": { term, input in
                guard case .true(_) = term else { XCTFail(); return }
            },
            "succ(succ(succ(0)))": { term, input in
                guard case .succ(.succ(.succ(.zero, _), _), _) = term else {
                    XCTFail(); return
                }
            },
            "succ(if false then 0 else succ(0))": { term, input in
                guard case .succ(.succ(.zero, _), _) = term else { XCTFail(); return }
            },
            "succ(if true then true else false)": { term, input in
                switch term {
                case .succ(.true, _): return
                default: XCTFail()
                }
            },
        ]
        for (input, expect) in inputs {
            let tokens = try lex(input)
            let syntax = try parse(tokens)
            let result = eval(syntax)
            expect(result, input)
        }
    }
}
