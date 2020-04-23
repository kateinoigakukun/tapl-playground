import XCTest
import TaPLPlayground
@testable import Chapter8

final class Chapter8Tests: XCTestCase {

    func testTypeof() throws {
        let inputs: [(source: String, type: Type, line: UInt)] = [
            (#"\t: Bool. t"#, .arrow(.bool, .bool), #line),
            (#"\t: Bool. \s: Bool. t"#, .arrow(.bool, .arrow(.bool, .bool)), #line),
        ]
        for input in inputs {
            do {
                let tokens = try lex(input.source)
                let syntax = try parse(tokens)
                let noname = removeNames(syntax)
                try XCTAssertEqual(typeof(noname), input.type, line: input.line)
            } catch {
                XCTFail(String(describing: error), line: input.line)
            }
        }
    }
}
