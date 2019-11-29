with import <nixpkgs> {};

runCommand "dummy" {
	buildInputs = [
    gcc
	];
} ""
