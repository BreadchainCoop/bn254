use commonware_codec::FixedSize;

mod bn254;

fn main() {
    println!("{}", bn254::Signature::SIZE);
}
