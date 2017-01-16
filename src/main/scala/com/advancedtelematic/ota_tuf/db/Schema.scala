package com.advancedtelematic.ota_tuf.db

import com.advancedtelematic.ota_tuf.data.KeyGenRequestStatus.KeyGenRequestStatus
import com.advancedtelematic.ota_tuf.data.KeyType.KeyType
import slick.driver.MySQLDriver.api._
import com.advancedtelematic.ota_tuf.data.DataType.KeyId._

object Schema {
  import com.advancedtelematic.ota_tuf.data.DataType._

  class KeyGenRequestTable(tag: Tag) extends Table[KeyGenRequest](tag, "key_gen_requests") {
    def id = column[KeyId]("key_id")
    def size = column[Int]("size")
    def status = column[KeyGenRequestStatus]("status")

    override def * = (id, status, size) <> ((KeyGenRequest.apply _).tupled, KeyGenRequest.unapply)
  }

  protected [db] val keyGenRequests = TableQuery[KeyGenRequestTable]

  class KeyTable(tag: Tag) extends Table[Key](tag, "keys") {
    def id = column[KeyId]("key_id")
    def keyType = column[KeyType]("key_type")
    def publicKey = column[String]("public_key")

    override def * = (id, keyType, publicKey) <> ((Key.apply _).tupled, Key.unapply)
  }

  protected [db] val keys = TableQuery[KeyTable]
}
