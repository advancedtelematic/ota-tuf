package com.advancedtelematic.tuf.reposerver.db


import com.advancedtelematic.libats.slick.db.SlickCirceMapper
import com.advancedtelematic.libtuf.data.ValidatedString
import com.advancedtelematic.libtuf.data.ValidatedString.{ValidatedString, ValidatedStringValidation}
import com.advancedtelematic.tuf.reposerver.data.RepositoryDataType.StorageMethod
import com.advancedtelematic.tuf.reposerver.data.RepositoryDataType.StorageMethod.StorageMethod
import slick.jdbc.MySQLProfile.api._

import scala.reflect.ClassTag

object SlickMappings {
  implicit val storageMethodMapping = MappedColumnType.base[StorageMethod, String](
    {
      case StorageMethod.CliManaged => "CliManaged"
      case StorageMethod.Managed => "Managed"
      case StorageMethod.Unmanaged => "Unmanaged"
    },
    {
      case "CliManaged" => StorageMethod.CliManaged
      case "Managed" => StorageMethod.Managed
      case "Unmanaged" =>  StorageMethod.Unmanaged
    }
  )
}

object SlickValidatedString {
  implicit def validatedStringMapper[W <: ValidatedString : ClassTag](implicit validation: ValidatedStringValidation[W]) = {
    implicit val decoder = ValidatedString.validatedStringDecoder[W]
    implicit val encoder = ValidatedString.validatedStringEncoder[W]
    SlickCirceMapper.circeMapper[W]
  }
}