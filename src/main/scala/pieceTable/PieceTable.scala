package pieceTable

import java.util.LinkedList
import scala.jdk.CollectionConverters.IterableHasAsScala

private case class Piece(
    val isOriginal: Boolean,
    var start: Int,
    var length: Int
)

// https://en.wikipedia.org/wiki/Piece_table
class PieceTable(init: String = ""):
  @inline private def initTable(): LinkedList[Piece] =
    val t = LinkedList[Piece]()
    if init.length != 0 then t.add(Piece(true, 0, init.length))
    t

  private val origBuf: String = init
  private val addBuf: StringBuilder = StringBuilder()
  private var table: LinkedList[Piece] = initTable()

  override def toString(): String =
    val out = StringBuilder()
    for piece <- table.asScala do
      if piece.isOriginal then
        out.addAll(origBuf.substring(piece.start, piece.start + piece.length))
      else out.addAll(addBuf.substring(piece.start, piece.start + piece.length))
    out.mkString

  def length: Int =
    table.asScala.map(_.length).sum()

  def tableSize: Int = table.size()

  def insert(pos: Int, s: String): Unit =
    val newPieceStart = addBuf.size
    addBuf.addAll(s)

    val it = table.listIterator()
    var piecePos = 0
    while it.hasNext() do
      val piece = it.next()
      if piecePos <= pos && pos < piecePos + piece.length then
        if piecePos == pos then
          it.previous()
          it.add(Piece(false, newPieceStart, s.length))
        else
          val firstChunkLen = pos - piecePos
          it.set(piece.copy(length = firstChunkLen))
          it.add(Piece(false, newPieceStart, s.length))
          it.add(
            piece.copy(
              start = piece.start + firstChunkLen,
              length = piece.length - firstChunkLen
            )
          )
        return
      piecePos += piece.length
    if pos != piecePos then throw IndexOutOfBoundsException(pos)

    if table.size() > 0 then
      val lastPiece = it.previous()
      it.next()
      if !lastPiece.isOriginal && lastPiece.start + lastPiece.length == newPieceStart
      then it.set(lastPiece.copy(length = lastPiece.length + s.length))
      else it.add(Piece(false, newPieceStart, s.length))
    else it.add(Piece(false, newPieceStart, s.length))

  def delete(pos: Int, count: Int): Unit =
    var toDelete = count
    val it = table.listIterator()
    var piecePos = 0
    while it.hasNext() do
      var piece = it.next()
      if piecePos <= pos && pos < piecePos + piece.length then
        if piecePos != pos then
          val firstChunkLen = pos - piecePos
          it.set(piece.copy(length = firstChunkLen))
          it.add(
            piece.copy(
              start = piece.start + firstChunkLen,
              length = piece.length - firstChunkLen
            )
          )
          piecePos += firstChunkLen
          // refresh iterator element for deletion
          it.previous()
          piece = it.next()
        if piece.length <= toDelete then
          it.remove()
          piecePos -= piece.length
          toDelete -= piece.length
          if toDelete == 0 then return
        else
          it.set(
            piece.copy(
              start = piece.start + toDelete,
              length = piece.length - toDelete
            )
          )
          return
      piecePos += piece.length
    throw IndexOutOfBoundsException(pos)
