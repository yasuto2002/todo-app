package lib.persistence

import akka.parboiled2.support
import cats.implicits.{catsKernelStdEqForOption, catsKernelStdHashForOption, catsSyntaxEq}
import ixias.model.EntityModel
import ixias.persistence.{SlickRepository, model}
import slick.jdbc.JdbcProfile
import ixias.persistence.SlickRepository
import lib.model.{Category, Todo, User}
import lib.persistence.db.{CategoryTable, TodoTable}
import slick.ast
import slick.ast.Join

import scala.concurrent.Future
import scala.util.Try

case class TodoRepository[P <: JdbcProfile]()(implicit val driver: P)
  extends SlickRepository[Todo.Id, Todo, P]
    with db.SlickResourceProvider[P] {

  import api._

  def get(id: Id): Future[Option[EntityEmbeddedId]] =
    RunDBAction(TodoTable, "slave") {
      _
        .filter(_.id === id)
        .result.headOption
    }

  def add(entity: EntityWithNoId): Future[Id] =
    RunDBAction(TodoTable) { slick =>
      slick returning slick.map(_.id) += entity.v
    }

  def update(entity: EntityEmbeddedId): Future[Option[EntityEmbeddedId]] =
    RunDBAction(TodoTable) { slick =>
      val row = slick.filter(_.id === entity.id)
      for {
        old <- row.result.headOption
        _ <- old match {
          case None => DBIO.successful(0)
          case Some(_) => row.update(entity.v)
        }
      } yield old
    }

  def remove(id: Id): Future[Option[EntityEmbeddedId]] = {
    RunDBAction(TodoTable) { slick =>
      val row = slick.filter(_.id === id)
      for {
        old <- row.result.headOption
        _ <- old match {
          case None => DBIO.successful(0)
          case Some(_) => row.delete
        }
      } yield old
    }
  }

  def all():Future[Seq[(Todo,Category)]] =
    RunDBAction(TodoTable, "slave") { slick =>
      slick.join(CategoryTable.query).on(_.category_id === _.id).result
    }
}
