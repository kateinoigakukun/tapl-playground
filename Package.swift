// swift-tools-version:5.2
// The swift-tools-version declares the minimum version of Swift required to build this package.

import PackageDescription

let package = Package(
    name: "TaPLPlayground",
    products: [
        .library(
            name: "TaPLPlayground",
            targets: ["TaPLPlayground"]),
    ],
    dependencies: [
        .package(name: "Curry", url: "https://github.com/thoughtbot/Curry.git", from: "4.0.2"),
    ],
    targets: [
        .target(name: "Chapter3", dependencies: ["TaPLPlayground"]),
        .testTarget(name: "Chapter3Tests", dependencies: ["Chapter3"]),
        .target(name: "Chapter5", dependencies: ["TaPLPlayground"]),
        .testTarget(name: "Chapter5Tests", dependencies: ["Chapter5"]),
        .target(
            name: "TaPLPlayground",
            dependencies: [
                .product(name: "Curry", package: "Curry")
            ]),
        .testTarget(
            name: "TaPLPlaygroundTests",
            dependencies: ["TaPLPlayground"]),
    ]
)
