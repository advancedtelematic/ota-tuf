package com.advancedtelematic.ota_tuf.data

import java.util.UUID

import cats.Show
import com.advancedtelematic.ota_tuf.data.UUIDKey.{UUIDKey, UUIDKeyObj}
import com.advancedtelematic.ota_tuf.data.KeyGenRequestStatus.KeyGenRequestStatus
import com.advancedtelematic.ota_tuf.data.KeyType.KeyType
import org.genivi.sota.data.SlickEnum
import slick.ast.BaseTypedType
import slick.driver.MySQLDriver.api._
import slick.jdbc.JdbcType

import scala.reflect.ClassTag

object KeyGenRequestStatus extends Enumeration with SlickEnum {
  type KeyGenRequestStatus = Value

  val REQUESTED, GENERATED, ERROR = Value
}

object KeyType extends Enumeration with SlickEnum {
  type KeyType = Value

  val RSA = Value
}

object UUIDKey {
  abstract class UUIDKeyObj {
    type Self <: UUIDKey

    implicit val abstractKeyShow = Show.show[Self](_.uuid.toString)

    def generate(): Self = fromJava(UUID.randomUUID())

    protected def fromJava(value: UUID): Self

    implicit def dbMapping(implicit ct: ClassTag[Self]): JdbcType[Self] with BaseTypedType[Self] =
      MappedColumnType.base[Self, String](_.uuid.toString, (s: String) => fromJava(UUID.fromString(s)))
  }

  abstract class UUIDKey {
    val uuid: UUID
  }
}

object DataType {
  object KeyId extends UUIDKeyObj {
    type Self = KeyId

    override protected def fromJava(value: UUID): KeyId = KeyId(value)
  }

  case class KeyId(uuid: UUID) extends UUIDKey

  case class KeyGenRequest(id: KeyId, status: KeyGenRequestStatus, size: Int = 512)

  case class Key(id: KeyId, keyType: KeyType, publicKey: String)
}
