signature RANDKEY =
sig
  type t
  val gen : t Gen.gen
  val cogen : (t, 'b) Gen.co
end
