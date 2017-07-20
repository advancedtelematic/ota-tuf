package com.advancedtelematic.libtuf.data

import com.advancedtelematic.libats.codecs.CirceRefined._
import com.advancedtelematic.libats.codecs.Codecs._
import com.advancedtelematic.libats.data.Namespace
import com.advancedtelematic.libats.messaging.Messages.MessageLike
import com.advancedtelematic.libats.messaging_datatype.DataType.TargetFilename
import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.libtuf.data.ClientDataType.TargetCustom
import com.advancedtelematic.libtuf.data.TufCodecs._
import com.advancedtelematic.libtuf.data.TufDataType.Checksum

object Messages {
  final case class TufTargetAdded(
    namespace: Namespace,
    filename: TargetFilename,
    checksum: Checksum,
    length: Long,
    custom: Option[TargetCustom])

  implicit val tufTargetAddedMessageLike = MessageLike[TufTargetAdded](_.namespace.get)
}
