#! /bin/sh
[ -z "$1" ] && (echo "error: supply an argument pls"; exit 1)
rustc test/$1.rs -o __test_bin || exit 1
./__test_bin || exit 1
rm ./__test_bin || exit 1
cargo run -- $1.rom
rm $1.rom
