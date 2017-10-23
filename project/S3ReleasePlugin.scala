package s3Release

import java.nio.file._
import com.amazonaws.auth.{AWSCredentials, AWSCredentialsProvider}
import com.amazonaws.regions.Regions
import com.amazonaws.services.s3.AmazonS3ClientBuilder
import sbt.Keys._
import sbt.{AutoPlugin, InputKey, Logger, SettingKey}

import scala.collection.JavaConversions._

object S3ReleasePlugin extends AutoPlugin {
  override lazy val projectSettings = s3ReleaseSettings

  object autoImport {
    lazy val s3UploadTask = InputKey[Unit]("s3release", "release artifacts to s3")
    lazy val s3Region = SettingKey[String]("s3Region")
    lazy val s3AccessKey = SettingKey[String]("s3AccessKey")
    lazy val s3SecretKey = SettingKey[String]("s3SecretKey")
    lazy val s3Bucket = SettingKey[String]("s3Bucket")
  }

  import autoImport._

  private def readEnv(envKey: String, default: Option[String] = None): String =
    sys.env.get(envKey).orElse(default).getOrElse(s"<undefined env $envKey>")

  import sbt.complete.DefaultParsers._

  val s3ReleaseSettings = Seq (
    s3Region := readEnv("AWS_REGION"),
    s3AccessKey := readEnv("AWS_ACCESS_KEY"),
    s3SecretKey := readEnv("AWS_SECRET_KEY"),
    s3Bucket := readEnv("AWS_BUCKET_ID"),
    s3Region := readEnv("AWS_REGION", Option(Regions.EU_CENTRAL_1.getName)),
    s3UploadTask := {
      val force = (token(Space) ~> token("force", "force release overwriting")).?.parsed.isDefined
      val log = streams.value.log
      val credentials = S3Credentials(s3AccessKey.value, s3SecretKey.value, s3Bucket.value, Regions.fromName(s3Region.value))
      val releaseUpload = new S3ReleaseUpload(log, credentials)

      releaseUpload.uploadLatest(force)
    }
  )
}


class S3ReleaseUpload(log: Logger, credentials: S3Credentials) {
  import java.nio.file.FileSystems

  lazy val s3Client =
    AmazonS3ClientBuilder.standard()
      .withCredentials(credentials)
      .withRegion(credentials.region)
      .build()

  def uploadLatest(force: Boolean = false) = {
    val releasePath = findLatestRelease(log)
    log.info(s"Using release $releasePath")
    uploadObject(releasePath, force)
  }

  private def findLatestRelease(log: Logger): Path = {
    val matcher = FileSystems.getDefault.getPathMatcher("glob:**/*.tgz")

    val allReleases =
      Files.list(Paths.get("cli/target/universal/"))
        .iterator()
        .filter(matcher.matches)
        .toVector
        .sortBy(_.getFileName.toString)(Ordering[String].reverse)

    log.info(s"Available releases: $allReleases")

    allReleases.headOption.getOrElse(throw new Exception("Could not find release to push to s3"))
  }

  private def uploadObject(path: Path, force: Boolean) = {
    val fileName = path.getFileName.toString

    val exists = s3Client.doesObjectExist(credentials.bucketId, fileName)

    if(exists && !force) {
      val url = s3Client.getUrl(credentials.bucketId, fileName)
      throw new Exception(s"release already exists in s3: $url")
    } else {
      if(force && exists)
        log.warn(s"Overwriting release $fileName")

      s3Client.putObject(credentials.bucketId, fileName, path.toFile)
      log.info(s"Uploaded file to s3 bucket ${credentials.bucketId}")
    }

    val url = s3Client.getUrl(credentials.bucketId, fileName)
    log.info(s"Url for $fileName release is $url")
  }
}

private case class S3Credentials(accessKey: String, secretKey: String, bucketId: String, region: Regions)
  extends AWSCredentials with AWSCredentialsProvider {

  override def getAWSAccessKeyId: String = accessKey

  override def getAWSSecretKey: String = secretKey

  override def refresh(): Unit = ()

  override def getCredentials: AWSCredentials = this
}
