package controllers

import ixias.model.tag
import lib.model.{Category, Todo}
import play.api.mvc.{AbstractController, ControllerComponents, MessagesActionBuilder}

import javax.inject.{Inject, Singleton}
import model.ViewValueTodoList
import lib.persistence.onMySQL.TodoRepository
import play.api.mvc.{AnyContent, MessagesRequest}
import lib.persistence.onMySQL.CategoryRepository

import scala.concurrent.{ExecutionContext, Future}
import play.api.data.Form
import play.api.data.Forms._
import play.api.data.validation.Constraints.{maxLength, nonEmpty}
import model.ViewValueHome
@Singleton
class TodoController @Inject()(messagesAction: MessagesActionBuilder, components: ControllerComponents)
(implicit executionContext: ExecutionContext)extends AbstractController(components){

  val todoForm: Form[Todo.WithNoId] = Form(
    mapping(
      "category" -> longNumber.transform[Category.Id]({id:Long => tag[Category][Long](id)},{categoryId:Category.Id => categoryId.toLong }),
      "title" -> text.verifying(nonEmpty).verifying(maxLength(255)),
      "body" -> text.verifying(nonEmpty).verifying(maxLength(255)),
      "state" -> shortNumber.transform[lib.model.Todo.Status]({Todo.Status(_)},{_.code}),
    )(Todo.apply)(Todo.unapply)
      .verifying(
      "Incorrect status!",
      fields => lib.model.Todo.Status.values.contains(fields.v.state)
    )
  )

  def index() = Action.async { implicit req =>
    val vv = ViewValueTodoList(
      title = "TODOリスト",
      cssSrc = Seq("main.css","todoList.css"),
      jsSrc = Seq("main.js")
    )
    TodoRepository.all().map(todos => {
      Ok(views.html.Todo.List(vv)(todos))
    })
  }

  def create() = messagesAction.async { implicit request: MessagesRequest[AnyContent] =>
    val vv = ViewValueTodoList(
      title = "TODOリスト",
      cssSrc = Seq("main.css", "todoForm.css"),
      jsSrc = Seq("main.js")
    )

    CategoryRepository.all().map(categories => {
      Ok(views.html.Todo.Create(vv)(todoForm)(categories))
    })
  }

  def store = messagesAction.async { implicit req =>
    val vv = ViewValueTodoList(
      title = "TODO追加",
      cssSrc = Seq("main.css", "todoForm.css"),
      jsSrc = Seq("main.js")
    )
    todoForm.bindFromRequest().fold(
      formWithErrors => {
        CategoryRepository.all().map(categories => {
          BadRequest(views.html.Todo.Create(vv)(formWithErrors)(categories))
        })
      },
      todo => {
        CategoryRepository.get(todo.v.category_id).flatMap(category => category
           match {
            case Some(_) =>
              TodoRepository.add(todo).map(_ => Redirect(routes.TodoController.index()))
            case None => CategoryRepository.all().map(categories => {
              BadRequest(views.html.Todo.Create(vv)(todoForm.withError("category","Invalid value"))(categories))
            })
          }
        )
      }
    )
  }

  def edit(todoId: Long) = messagesAction.async { implicit req =>
    val vv = ViewValueTodoList(
      title = "Todo編集",
      cssSrc = Seq("main.css", "todoForm.css"),
      jsSrc = Seq("main.js")
    )
    CategoryRepository.all().flatMap { categories =>
      TodoRepository.get(tag[Todo][Long](todoId)).map(todo => {
        todo match {
          case Some(value) => {
            val filledForm = todoForm.fill(Todo(value.v.category_id, value.v.title, value.v.body, value.v.state))
            Ok(views.html.Todo.Edit(vv)(filledForm)(categories)(todoId))
          }
          case None => BadRequest(views.html.Todo.Create(vv)(todoForm.withError("category","Invalid value"))(categories))
        }
      })
    }
  }

  def update(todoId: Long) = messagesAction.async { implicit req =>

    val vv = ViewValueTodoList(
      title = "Todo編集",
      cssSrc = Seq("main.css", "todoForm.css"),
      jsSrc = Seq("main.js")
    )
    todoForm.bindFromRequest().fold(
      formWithErrors => {
        CategoryRepository.all().map(categories => {
          BadRequest(views.html.Todo.Edit(vv)(formWithErrors)(categories)(todoId))
        })
      },
      todoReq => {
        for{
          todoCheck <- TodoRepository.get(tag[Todo][Long](todoId)) // id確認
          categoryCheck <- CategoryRepository.get(todoReq.v.category_id) // category確認
          result <- (todoCheck, categoryCheck) match {
            case (Some(todo), Some(_)) => {
              // 更新
              val copyTodo: Todo.EmbeddedId = todo.v.copy(category_id = todoReq.v.category_id, state = todoReq.v.state, title = todoReq.v.title, body = todoReq.v.body).toEmbeddedId
              TodoRepository.update(copyTodo).map(_.fold{InternalServerError("Server Error")}{_ => Redirect(routes.TodoController.index())})
            }
            case (_, _) => {
               Future.successful(BadRequest("Invalid value"))
            }
          }
        }yield result
      }
    )
  }
}