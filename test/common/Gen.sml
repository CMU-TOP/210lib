structure Gen =
let structure QGen = QCheck.Gen in
struct
  type 'a gen = 'a QGen.gen
  type ('a, 'b) co = ('a, 'b) QGen.co

  val choose = QGen.chooseL
  val choose' = QGen.chooseL'
  val range = QGen.range
  val lift = QGen.lift
  val map = QGen.map
  fun bind g f r = let val (a, r) = g r in f a r end

  (* base type generators *)
  val int = QGen.Int.int
  val bool = QGen.flip
  val char = QGen.char
  val unit = lift ()
  (* list generators that stops with probability 1/8 *)
  fun list elem = QGen.list (QGen.flip' (1,7)) elem
  val string = QGen.map String.implode (list char)
  val safeLen = QGen.map List.length (list unit)
  val len = choose' [(4, safeLen), (1, QGen.Int.neg)]
  fun safeIndex len = range (0, len - 1)
  fun index' 0 = choose [int, lift (~1), lift 0]
    | index' len = choose'
        [ (4, safeIndex len)
        , (1, choose [int, lift (~1), lift len])
        ]
  val index = bind safeLen index'
  (* pair generators *)
  val pair = QGen.zip
  val pair3 = QGen.zip3
  (* cogens and functions *)
  val coint = QCheck.Gen.Int.coint
  val cobool = QCheck.Gen.cobool
  fun copair (ga, gb) (a, b) = gb b o ga a
  val function = QGen.arrow
end
end
