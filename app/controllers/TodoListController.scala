package controllers

import models.{NewTodoListItem, TodoListItem}
import play.api.libs.json._

import javax.inject._
import play.api._
import play.api.mvc._

import scala.collection.mutable

@Singleton
class TodoListController @Inject()(val controllerComponents: ControllerComponents) extends BaseController {
  implicit val todoListJson = Json.format[TodoListItem]
  implicit val newTodoListJson = Json.format[NewTodoListItem]

  private val todoList = new mutable.ListBuffer[TodoListItem]()
  todoList += TodoListItem(1, "Test", true)
  todoList += TodoListItem(2, "some other value", false)

  def getAll(): Action[AnyContent] = Action {
    if (todoList.isEmpty) {
      NoContent
    } else {
      Ok(Json.toJson(todoList))
    }
  }

  def getById(itemId: Long) = Action{
    val foundItem = todoList.find(_.id==itemId)
    foundItem match{
      case Some(item)=>Ok(Json.toJson(item))
      case None => NotFound
    }
  }
  def addNewItem() = Action{ implicit request =>
    val content = request.body
    val jsonObject = content.asJson
    val todoListItem :Option[NewTodoListItem] =
      jsonObject.flatMap(
        Json.fromJson[NewTodoListItem](_).asOpt
      )

    todoListItem match {
      case Some(newItem)=>
        val nextId=todoList.map(_.id).max +1
        val tobeAdded=TodoListItem(nextId, newItem.description, false)
        todoList +=tobeAdded
        Created(Json.toJson(tobeAdded))
      case None =>
        BadRequest
    }
  }

  def markAsDone(itemId: Long) = Action {
    val foundItem = todoList.find(_.id == itemId)
    foundItem match {
      case Some(item) =>
        val newItem = item.copy(isItDone = true)
        todoList.dropWhileInPlace(_.id == itemId)
        todoList += newItem
        Accepted(Json.toJson(newItem))
      case None => NotFound
    }
  }

  def deleteAllDone() = Action {
    todoList.filterInPlace(_.isItDone == false)
    Accepted
  }

}