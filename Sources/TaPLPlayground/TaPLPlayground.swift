@_exported import Curry

struct TaPLPlayground {
    var text = "Hello, World!"
}

public func unimplemented(_ message: String = "",
                          file: StaticString = #file, line: UInt = #line) -> Never {
    fatalError(message, file: file, line: line)
}
