package com.advancedtelematic.tuf.reposerver.db

import com.advancedtelematic.libats.slick.codecs.SlickEnumMapper
import com.advancedtelematic.tuf.reposerver.data.RepositoryDataType.StorageMethod

object SlickMappings {
  implicit val storageMethodMapping = SlickEnumMapper.enumMapper(StorageMethod)
}
