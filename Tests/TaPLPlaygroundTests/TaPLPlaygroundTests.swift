import XCTest
@testable import TaPLPlayground

final class TaPLPlaygroundTests: XCTestCase {
    func testExample() {
        // This is an example of a functional test case.
        // Use XCTAssert and related functions to verify your tests produce the correct
        // results.
        XCTAssertEqual(TaPLPlayground().text, "Hello, World!")
    }

    static var allTests = [
        ("testExample", testExample)
    ]
}
