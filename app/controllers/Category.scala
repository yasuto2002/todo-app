package controllers

import com.mysql.jdbc.exceptions.jdbc4.MySQLIntegrityConstraintViolationException
import ixias.model.tag
import lib.model.Category
import lib.model.Category.Color
import lib.persistence.onMySQL.CategoryRepository
import play.api.data.Form
import play.api.data.Forms.{mapping, shortNumber, text}
import play.api.data.validation.Constraints.{maxLength, nonEmpty, pattern}

import javax.inject._
import play.api.mvc._

import scala.concurrent.{ExecutionContext, Future}


@Singleton
class CategoryController @Inject()(messagesAction: MessagesActionBuilder, components: ControllerComponents)
                                  (implicit executionContext: ExecutionContext)extends AbstractController(components){
  val categoryForm:Form[Category.WithNoId] = Form(
    mapping(
      "name"  -> text.verifying(nonEmpty).verifying(maxLength(255)).verifying(pattern(("^[0-9a-zA-Zぁ-んーァ-ンヴー一-龠]*$").r,error = "英数字・日本語のみ入力できます")),
      "slug"  -> text.verifying(nonEmpty).verifying(maxLength(64)).verifying(pattern(("^[0-9a-zA-Z]*$").r,error = "英数字のみ入力できます")),
      "color" -> shortNumber.transform[Color](Color(_),_.code)
    )(Category.apply)(Category.unapply)
      .verifying(
        "Incorrect status!",
        fields => Category.Color.values.contains(fields.v.color)
      )
  )

  def index() = Action.async { implicit req =>
    val vv = model.ViewValueCategory(
      title  = "Category一覧",
      cssSrc = Seq("main.css","categoryList.css"),
      jsSrc  = Seq("main.js")
    )
    CategoryRepository.all().map(categories => Ok(views.html.Category.List(vv)(categories)))
  }

  def create() = messagesAction { implicit req =>
    val vv = model.ViewValueCategory(
      title = "Category追加",
      cssSrc = Seq("main.css", "categoryForm.css"),
      jsSrc = Seq("main.js")
    )
    Ok(views.html.Category.Create(vv)(categoryForm))
  }

  def store() = messagesAction.async { implicit req =>
    val vv = model.ViewValueCategory(
      title = "Category追加",
      cssSrc = Seq("main.css", "categoryForm.css"),
      jsSrc = Seq("main.js")
    )
    categoryForm.bindFromRequest().fold(
      formWithErrors => {
        Future.successful(BadRequest(views.html.Category.Create(vv)(formWithErrors)))
      },
      categoryNoId => {
        CategoryRepository.add(categoryNoId).map(_ => Redirect(routes.CategoryController.index()))
      }
    )
  }

  def edit(categoryId:Long) = messagesAction.async { implicit req =>
    val vv = model.ViewValueCategory(
      title = "Category編集",
      cssSrc = Seq("main.css", "categoryForm.css"),
      jsSrc = Seq("main.js")
    )
    CategoryRepository.get(tag[Category][Long](categoryId)).map{ value =>
      value.fold(Redirect(routes.CategoryController.index()))
      {category =>
        val fillCategory = categoryForm.fill(Category(category.v.name,category.v.slug,category.v.color))
        Ok(views.html.Category.Edit(vv)(fillCategory)(categoryId))
      }
    }
  }

  def update(categoryId:Long) = messagesAction.async { implicit req =>
    val vv = model.ViewValueCategory(
      title = "Category編集",
      cssSrc = Seq("main.css", "categoryForm.css"),
      jsSrc = Seq("main.js")
    )
    categoryForm.bindFromRequest().fold(
      formWithErrors => {
        Future{ Ok(views.html.Category.Edit(vv)(formWithErrors)(categoryId))}
      },
      categoryReq => {
        CategoryRepository.get(tag[Category][Long](categoryId)).flatMap{ _ match {
          case Some(category) => {
            val copyCategory = category.v.copy(name = categoryReq.v.name,slug = categoryReq.v.slug,color = categoryReq.v.color).toEmbeddedId
            CategoryRepository.update(copyCategory).map{_.fold{InternalServerError("Server Error")}{_ => Redirect(routes.CategoryController.index())}}
          }
          case None => {
            Future.successful(BadRequest("The specified category does not exist"))
          }
        }}
      }
    )
  }

  def delete(categoryId:Long) = messagesAction.async { implicit req =>
    CategoryRepository.get(tag[Category][Long](categoryId)).flatMap(_ match {
      case Some(category) => {
          CategoryRepository.cascadeDelete(category.id).map(_.fold{InternalServerError("Server Error")}{_ => Redirect(routes.CategoryController.index())})
      }
      case None => {
        Future.successful(BadRequest("The specified category does not exist"))
      }
    })
  }
}
