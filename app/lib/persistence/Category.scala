package lib.persistence

import ixias.persistence.SlickRepository
import lib.model
import lib.model.Category
import lib.persistence.onMySQL.CategoryRepository.EntityEmbeddedId
import slick.jdbc.JdbcProfile
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.Future
case class CategoryRepository[P <: JdbcProfile]()(implicit val driver: P)
  extends SlickRepository[Category.Id, Category, P]
    with db.SlickResourceProvider[P] {

  import api._

  def get(id: Id): Future[Option[EntityEmbeddedId]] =
    RunDBAction(CategoryTable, "slave") {
      _
        .filter(_.id === id)
        .result.headOption
    }

  def add(entity: EntityWithNoId): Future[Id] =
    RunDBAction(CategoryTable) { slick =>
      slick returning slick.map(_.id) += entity.v
    }

  def update(entity: EntityEmbeddedId): Future[Option[EntityEmbeddedId]] =
    RunDBAction(CategoryTable) { slick =>
      val row = slick.filter(_.id === entity.id)
      for {
        old <- row.result.headOption
        _ <- old match {
          case None => DBIO.successful(0)
          case Some(_) => row.update(entity.v)
        }
      } yield old
    }

  def remove(id: Id): Future[Option[EntityEmbeddedId]] =
    RunDBAction(CategoryTable) { slick =>
      val row = slick.filter(_.id === id)
      for {
        old <- row.result.headOption
        _ <- old match {
          case None => DBIO.successful(0)
          case Some(_) => row.delete
        }
      } yield old
    }

  def cascadeDelete(id: Id): Future[Option[EntityEmbeddedId]] =
      RunDBAction(TodoTable) { slick =>
        val todos = slick.filter(_.category_id === id)
        todos.delete
      }.flatMap { _ =>
        remove(id)
      }


  def all(): Future[Seq[EntityEmbeddedId]] =
    RunDBAction(CategoryTable, "slave") { slick =>
      slick.result
    }
}