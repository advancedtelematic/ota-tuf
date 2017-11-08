package com.advancedtelematic.tuf.keyserver.daemon

import akka.actor.Status.{Failure, Success}
import akka.actor.{Actor, ActorLogging, Props, Status, SupervisorStrategy}
import akka.routing.RoundRobinPool
import cats.syntax.show.toShowOps
import com.advancedtelematic.tuf.keyserver.daemon.KeyGeneratorLeader.Tick
import com.advancedtelematic.tuf.keyserver.data.KeyServerDataType._
import com.advancedtelematic.libtuf.data.TufDataType._
import com.advancedtelematic.tuf.keyserver.vault.VaultClient
import com.advancedtelematic.tuf.keyserver.vault.VaultClient.VaultKey
import slick.jdbc.MySQLProfile.api._
import com.advancedtelematic.libtuf.crypt.TufCrypto
import com.advancedtelematic.libtuf.crypt.TufCrypto._
import scala.async.Async._
import scala.concurrent.duration._
import akka.pattern.pipe
import com.advancedtelematic.tuf.keyserver.data.KeyServerDataType.KeyGenRequestStatus
import com.advancedtelematic.tuf.keyserver.db.{KeyGenRequestSupport, KeyRepositorySupport, RoleRepositorySupport}
import org.slf4j.LoggerFactory

import scala.concurrent.{ExecutionContext, Future}


object KeyGeneratorLeader {
  case object Tick

  def props(vaultClient: VaultClient)(implicit db: Database): Props = Props(new KeyGeneratorLeader(vaultClient))
}

class KeyGeneratorLeader(vaultClient: VaultClient)(implicit val db: Database) extends Actor with ActorLogging with KeyGenRequestSupport {

  implicit val ec = context.dispatcher

  private val WORKER_COUNT = 10

  override def preStart(): Unit = {
    self ! Tick
  }

  private val router = {
    val routerProps = RoundRobinPool(WORKER_COUNT)
      .withSupervisorStrategy(SupervisorStrategy.defaultStrategy)
      .props(KeyGeneratorWorker.props(vaultClient))

     context.system.actorOf(routerProps)
   }

  def waiting(totalTasks: Int, remaining: Int): Receive =
    if(remaining == 0) {
      val nextInterval = if(totalTasks > 0) 0.seconds else 3.seconds
      log.info("Finished generating {} keys", totalTasks)
      context.system.scheduler.scheduleOnce(nextInterval, self, Tick)
      receive
    } else {
      {
        case Status.Success(_) =>
          context.become(waiting(totalTasks, remaining - 1))
        case Status.Failure(ex) =>
          log.error(ex, "Could not generate key")
          context.become(waiting(totalTasks, remaining - 1))
      }
    }


  override def receive: Receive = {
    case Status.Failure(ex) =>
      throw ex

    case taskCount: Int =>
      log.info("Waiting for {} key generation tasks to complete", taskCount)
      context become waiting(taskCount, taskCount)

    case Tick =>
      log.info("Tick")

      val f = keyGenRepo.findPending(limit = WORKER_COUNT * 4).map { m =>
        m.foreach { router ! _ }
        m.size
      }

      f.pipeTo(self)
  }
}


class KeyGenerationOp(vaultClient: VaultClient)(implicit val db: Database, val ec: ExecutionContext)
  extends KeyGenRequestSupport
    with KeyRepositorySupport
    with RoleRepositorySupport {

  private lazy val _log = LoggerFactory.getLogger(this.getClass)

  protected def generateKeys(roleId: RoleId, keyType: KeyType, keySize: Int, threshold: Int): Seq[(Key, TufPrivateKey)] = {
    require(threshold > 0, "threshold must be greater than 0")

    (0 until threshold).map { _ =>
      val pair = TufCrypto.generateKeyPair(keyType, keySize)
      (Key(pair.pubkey.id, roleId, keyType, pair.pubkey.keyval), pair.privkey)
    }
  }

  protected def saveToVault(keys: Seq[(Key, TufPrivateKey)]): Future[Unit] = {
    Future.traverse(keys) { case (key, privateKey) =>
      val vaultKey = VaultKey(key.id, key.keyType, key.publicKey.toPem, privateKey)
      vaultClient.createKey(vaultKey)
    }.map(_ => ())
  }

  def processGenerationRequest(kgr: KeyGenRequest): Future[Seq[Key]] =
    async {
      val role = Role(RoleId.generate(), kgr.repoId, kgr.roleType, kgr.threshold)

      val keyPairs = generateKeys(role.id, kgr.keyType, kgr.keySize, kgr.threshold)
      val keys = keyPairs.map(_._1)

      await(saveToVault(keyPairs))
      await(keyGenRepo.persistGenerated(kgr, keys, role, keyRepo, roleRepo))

      _log.info("Generated keys {}", keys.map(_.id.value))
      keys
    }
}

object KeyGeneratorWorker {
  def props(vaultClient: VaultClient)(implicit db: Database): Props = Props(new KeyGeneratorWorker(vaultClient))
}

class KeyGeneratorWorker(vaultClient: VaultClient)(implicit val db: Database) extends Actor
  with ActorLogging
  with KeyGenRequestSupport
  with KeyRepositorySupport
  with RoleRepositorySupport {

  implicit val ec = context.dispatcher

  val keyGenerationOp = new KeyGenerationOp(vaultClient)

  override def receive: Receive = {
    case kgr: KeyGenRequest =>
      log.info(s"Received key gen request for {}", kgr.id.show)

      keyGenerationOp.processGenerationRequest(kgr)
        .map(Success)
        .recoverWith {
          case ex =>
            log.error("Key generation failed: {}", ex.getMessage)
            keyGenRepo
              .setStatus(kgr.id, KeyGenRequestStatus.ERROR, Option(ex))
              .map(_ => Failure(ex))
        }.pipeTo(sender)
  }
}
