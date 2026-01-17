// swift-tools-version:5.3
import PackageDescription

let package = Package(
    name: "TreeSitterCommandCadModel",
    products: [
        .library(name: "TreeSitterCommandCadModel", targets: ["TreeSitterCommandCadModel"]),
    ],
    dependencies: [
        .package(url: "https://github.com/ChimeHQ/SwiftTreeSitter", from: "0.8.0"),
    ],
    targets: [
        .target(
            name: "TreeSitterCommandCadModel",
            dependencies: [],
            path: ".",
            sources: [
                "src/parser.c",
                // NOTE: if your language has an external scanner, add it here.
            ],
            resources: [
                .copy("queries")
            ],
            publicHeadersPath: "bindings/swift",
            cSettings: [.headerSearchPath("src")]
        ),
        .testTarget(
            name: "TreeSitterCommandCadModelTests",
            dependencies: [
                "SwiftTreeSitter",
                "TreeSitterCommandCadModel",
            ],
            path: "bindings/swift/TreeSitterCommandCadModelTests"
        )
    ],
    cLanguageStandard: .c11
)
