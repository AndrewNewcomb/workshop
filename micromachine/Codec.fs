namespace Micromachine

open System
open System.Text

/// An decoder to from type 'i and encoder to type 'o for type 'a.
type Codec<'i, 'o, 'a> = ('i -> 'a option) * ('a -> 'o)

/// A codec for raw type 'm to type 'a.
type Codec<'m, 'a> = Codec<'m, 'm, 'a>

/// A codec between a type 'a and a byte array.
type ByteCodec<'a> = Codec<byte[], 'a>

/// A codec between a type 'a and a string.
type StringCodec<'a> = Codec<string, 'a>

/// An decoder to from type 'i and encoder to type 'o for type 'a where encoding may be an IO-bound
/// operation. Decoding errors are represented as strings.
type AsyncCodec<'i, 'o, 'a> = ('i -> Async<Choice<'a, string>>) * ('a -> Async<'o>)

/// Operations on codecs.
module Codec =

  /// Encodes a value using a codec.
  let inline encode (c:Codec<_, 'o, 'a>) a : 'o =
    let (_,enc) = c in
    enc a

  /// Decodes a value using a codec.
  let inline decode (c:Codec<'i, _, 'a>) i : 'a option =
    let (dec,_) = c in
    dec i

  /// Creates a decoder based on a set of decoders picking the result of the first
  /// to produce a value.
  let decodeAll (decs:('i -> 'a option) seq) : 'i -> 'a option =
    fun input -> decs |> Seq.tryPick ((|>) input)

  /// Maps over the encoded value.
  let mapEnc (f:'o -> 'o2) (c:Codec<'i, 'o, 'a>) : Codec<'i, 'o2, 'a> =
    let dec,enc = c in
    dec,(enc >> f)

  /// Maps over the input to the decoder.
  let mapDec (f:'i2 -> 'i) (c:Codec<'i, 'o, 'a>) : Codec<'i2, 'o, 'a> =
    let dec,enc = c in
    let dec = f >> dec
    dec,enc

  /// Maps over the input to the decoder and the output of the decoder.
  let dimap (f:'i2 -> 'i) (g:'o -> 'o2) (c:Codec<'i, 'o, 'a>) : Codec<'i2, 'o2, 'a> =
    let (dec,enc) = c in
    (f >> dec,enc >> g)

  /// Converts a byte codec to a string codec using UTF8.
  let byteToStringUTF8 (c:ByteCodec<'a>) : StringCodec<'a> =
    c |> dimap (Encoding.UTF8.GetBytes) (Encoding.UTF8.GetString)

  /// Converts a string codec to a byte codec using UTF8.
  let stringToByteUTF8 (c:StringCodec<'a>) : ByteCodec<'a> =
    c |> dimap (Encoding.UTF8.GetString) (Encoding.UTF8.GetBytes)


/// Operations on async codecs.
module AsyncCodec =

  /// Creates an async codec from a codec.
  let ofCodec (c:Codec<'i, 'o, 'a>) : AsyncCodec<'i, 'o, 'a> =
    let (dec,enc) = c in
    (dec >> (function Some a -> Choice1Of2 a | None -> Choice2Of2 "decoding error") >> async.Return, enc >> async.Return)

  /// Encodes a value using a codec.
  let inline encode (c:Codec<_, 'o, 'a>) a : 'o =
    let (_,enc) = c in
    enc a

  /// Decodes a value using a codec.
  let inline decode (c:Codec<'i, _, 'a>) i : 'a option =
    let (dec,_) = c in
    dec i

  /// Maps over the input to the decoder and the output of the decoder.
  let dimapAsync (f:'i2 -> Async<'i>) (g:'o -> Async<'o2>) (c:AsyncCodec<'i, 'o, 'a>) : AsyncCodec<'i2, 'o2, 'a> =
    let (dec,enc) = c
    let bind f a = async.Bind(a, f)
    (f >> bind dec, enc >> bind g)


[<AutoOpen>]
module ByteCodec =

  /// Encodes a value to a byte array using a ByteCodec provided by a static member on the encoded type.
  let inline encToBytesT (a:^a) =
    let codec = (^a : (static member ByteCodec : ByteCodec< ^a>) ()) in
    Codec.encode codec a

  /// Decodes a value from a byte array using a ByteCodec provided by a static member on the decoded type.
  let inline decFromBytesT (bytes:byte[]) =
    let codec = (^a : (static member ByteCodec : ByteCodec< ^a>) ()) in
    Codec.decode codec bytes