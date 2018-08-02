structure DumbSTListSequence : ST_SEQUENCE =
struct
  structure Seq = DumbListSequence

  open DumbListSequence

  type 'a stseq = 'a seq
  fun fromSeq l = l
  fun toSeq l = l
end
