import XCTest
import TaPLPlayground
@testable import Chapter5

final class Chapter5Tests: XCTestCase {
    func testParse() throws {
        let inputs: [String: (Term, String) -> Void] = [
            #"\x.t"#: { term, input in
                guard case .lambda(parameter: "x",
                                   body: .variable("t", _), _) = term else {
                    XCTFail(); return
                }
            },
            #"(t t)"#: { term, input in
                guard case .apply(.variable("t", _), .variable("t", _), _) = term else {
                    XCTFail(); return
                }
            },
            #"\x.\y. ((x y) x)"#: { term, input in
                guard case .lambda(
                    parameter: "x",
                    body: .lambda(
                        parameter: "y",
                        body: .apply(
                            .apply(
                                .variable("x", _),
                                .variable("y", _), _),
                            .variable("x", _),
                            _),
                        _),
                    _) = term else {
                        XCTFail(); return
                }
            }
        ]
        for (input, expect) in inputs {
            let tokens = try lex(input)
            let syntax = try parse(tokens)
            expect(syntax, input)
        }
    }

    func testRemoveNames() throws {
        let inputs: [String: String] = [
            /* plus: */ #"\m.\n.\s.\z. (m (s (n (s z))))"#: #"\. \. \. \. (3 (1 (2 (1 0))))"#
        ]

        for (input, expect) in inputs {
            let tokens = try lex(input)
            let syntax = try parse(tokens)
            let noname = removeNames(syntax)
            XCTAssertEqual(noname.dump(), expect)
        }
    }

    func testPrintTerm() throws {
        let inputs: [String: String] = [
            /* plus: */ #"\m.\n.\s.\z. (m (s (n (s z))))"#: "(lambda m. (lambda n. (lambda s. (lambda z. (m (s (n (s z))))))))"
        ]

        for (input, expect) in inputs {
            let tokens = try lex(input)
            let syntax = try parse(tokens)
            let noname = removeNames(syntax)
            let printed = printTerm(noname, context: [])
            XCTAssertEqual(printed, expect)
        }
    }

    func testEval() throws {
        let tru = #"\t. \f. t"#
        let fls = #"\t. \f. f"#
        let and = #"\b. \c. ((b c) \#(fls))"#
        let or  = #"\b. \c. ((b \#(tru)) c)"#
        let pair = #"\f. \s. \b. ((b f) s)"#
        let fst = #"\p. (p \#(tru))"#
        let scd = #"\p. (p \#(fls))"#

        let c0 = #"\s. \z. z"#
        let c1 = #"\s. \z. (s z)"#
        let c2 = #"\s. \z. (s (s z))"#
        let c3 = #"\s. \z. (s (s (s z)))"#
        let c5 = #"\s. \z. (s (s (s (s (s z)))))"#
        let c6 = #"\s. \z. (s (s (s (s (s (s z))))))"#

        let iszero = #"\n. ((n \x. \#(fls)) \#(tru))"#

        // let succ = #"\n. \s. \z. (s ((n s) z)"#
        let plus = #"\m. \n. \s. \z. ((m s) ((n s) z))"#

        let zz = #"((\#(pair) \#(c0)) \#(c0))"#
        let ss = #"\p. ((\#(pair) (\#(scd) p)) ((\#(plus) \#(c1)) (\#(scd) p)))"#
        let prd = #"\m. (\#(fst) ((m \#(ss)) \#(zz)))"#

        let times = #"\m. \n. ((m (\#(plus) n)) \#(c0))"#
        let sub = #"\m. \n. ((n \#(prd)) m)"#

        let equal = #"\m. \n. (\#(iszero) ((\#(sub) n) m))"#

        // let ogma = #"(\x. (x x) \x. (x x))"#
        let fixA = #"\x. (f \y. ((x x) y))"#
        let fix = #"\f. (\#(fixA) \#(fixA))"#
        // Note: Can't stop because `f` is evaluated before switching.
        //       Use primitive `if` instead of church bool to eval lazily.
        let _factorial = #"\f. \n. ( ( (\#(iszero) n) \#(c1)) ( (\#(times) n) (f (\#(prd) n))))"#
        let factorial = #"(\#(fix) \#(_factorial))"#
        _ = factorial

        func assertTrue(_ term: NoNameTerm) {
            guard case .lambda(_, .lambda(_, .variable(1, _, _), _), _) = term else {
                XCTFail(); return
            }
        }

        func assertFalse(_ term: NoNameTerm) {
            guard case .lambda(_, .lambda(_, .variable(0, _, _), _), _) = term else {
                XCTFail(); return
            }
        }

        let inputs: [String: (NoNameTerm) -> Void] = [
            #"(\x. \y. (\z. x x) \z. z)"#: { term in
                print(printTerm(term))
            },
            #"((\#(and) \#(tru)) \#(fls))"#: { term in
                assertFalse(term)
            },
            #"((\#(and) \#(fls)) \#(tru))"#: { term in
                assertFalse(term)
            },
            #"((\#(and) \#(tru)) \#(tru))"#: { term in
                assertTrue(term)
            },
            #"((\#(and) \#(fls)) \#(fls))"#: { term in
                assertFalse(term)
            },
            #"((\#(or) \#(tru)) \#(fls))"#: { term in
                assertTrue(term)
            },
            #"((\#(equal) ((\#(plus) \#(c2)) \#(c3))) \#(c5))"#: { term in
                assertTrue(term)
            },
            #"((\#(equal) ((\#(times) \#(c2)) \#(c3))) \#(c6))"#: { term in
                assertTrue(term)
            }
//            #"((\#(equal) (\#(factorial) \#(c3))) \#(c6))"#: { term in
//                print(printTerm(term))
//            }
        ]

        for (input, expect) in inputs {
            let tokens = try lex(input)
            let syntax = try parse(tokens)
            let noname = removeNames(syntax)
            let result = eval(noname)
            XCTAssertFalse(printTerm(result).contains("[bad index]"))
            expect(result)
        }
    }
}
