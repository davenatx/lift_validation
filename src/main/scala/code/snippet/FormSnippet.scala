package code.snippet

import net.liftweb.util._
import net.liftweb.common.Loggable
import Helpers._
import net.liftweb.http.SHtml
import net.liftweb.http.js.JE._
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js.jquery.JqJsCmds._
import net.liftweb.http.RequestVar

import java.util.regex.Pattern

/**
 * JavaScript for Twiter Bootstrap Feedback. Feedback messages are displayed using Tooltips.
 */
object BootstrapValidationHelpers extends Loggable {
  /* Bootstrap3 Sucess */
  case class ValidationSuccess(id: String) extends JsCmd {
    def toJsCmd = "$('" + id + "').removeClass().addClass('form-group has-success has-feedback')"
  }

  /* Bootstrap3 Success Feedback - green check mark */
  case class ValidationSuccessFeedback(id: String) extends JsCmd {
    def toJsCmd = "$('" + id + " span').removeClass().addClass('form-control-feedback glyphicon glyphicon-ok')"
  }

  /* Bootstrap3 Error */
  case class ValidationError(id: String) extends JsCmd {
    def toJsCmd = "$('" + id + "').removeClass().addClass('form-group has-error has-feedback')"
  }

  /* Bootstrap3 Error Feedback - red x */
  case class ValidationErrorFeedback(id: String) extends JsCmd {
    def toJsCmd = "$('" + id + " span').removeClass().addClass('form-control-feedback glyphicon glyphicon-remove')"
  }

  /* Bootstrap3 Message Feedback - tool tip title */
  case class ValidationMessage(id: String, element: String, message: String) extends JsCmd {
    def toJsCmd = "$('" + id + " " + element + "').attr('data-original-title', '" + message + "')"
  }

  /* Bootstrap3 Show Tool Tip */
  case class ValidationShowMessage(id: String, element: String) extends JsCmd {
    def toJsCmd = "$('" + id + " " + element + "').tooltip({trigger: 'manual'}).tooltip('show')"
  }

  /* Bootstrap3 Hide Tool Tip */
  case class ValidationHideMessage(id: String, element: String) extends JsCmd {
    def toJsCmd = "$('" + id + " " + element + "').tooltip('destroy')"
  }

  /* Bootstrap3 Reset validation */
  case class ValidationReset(id: String) extends JsCmd {
    def toJsCmd = "$('" + id + "').removeClass().addClass('form-group')"
  }

  /* Bootstrap3 Reset Feedback */
  case class ValidationResetFeedback(id: String) extends JsCmd {
    def toJsCmd = "$('" + id + " span').removeClass().addClass('form-control-feedback')"
  }
}

import BootstrapValidationHelpers._

/**
 * Field types that use BootstrapValidationHelpers.
 *
 * Each class takes a parameter named id.  This id is the div that contains the bootstrap form-control class.  It is also the div
 * that wraps the underlying field.
 *
 * Each class takes a parameter named element.  This is the specific type of element feedback is provided for.  For example,
 * if it is an <input>, input is used.  If it is a <textarea>, textarea is used.  These elements are used to ensure the
 * validation feedback targets the proper element.
 */
object FieldValidationHelpers extends Loggable {

  /* Helper to Clear element value targeted by outer div and type */
  case class ClearValue(id: String, element: String) extends JsCmd {
    def toJsCmd = "$('" + id + " " + element + "').val('')"
  }

  /* The base Field trait */
  trait Field {
    def isValid(input: String): Boolean
    def jsValidation(input: String): JsCmd
    def reset: JsCmd
  }

  /**
   * The use case for this function is to perform sever side validation AND client side validation in one place. For example, use this when
   * processing a form submission.  If validation fails on any fields, feedback is provided to the client.
   *
   * It returns None if there were no Validation errors or Some(JsCmd) if Validation errors occured
   *
   * Please note, the fields list and the inputs list must be in the same order.  Specifically, if the @name field is first entry in the fields
   * list, then the nameVar should be the first entry in the inputs list.
   */
  def validateFields(fields: List[Field], inputs: List[String]) = {
    val list = fields.zip(inputs)

    val results = list map (x => {
      if (x._1.isValid(x._2)) None
      else Some((x._1.jsValidation(x._2)))
    })

    val flatList = results.flatten

    flatList match {
      case List() => None
      case _ => Some(flatList.foldLeft(Noop) { (z, f) => z.&(f) })
    }
  }

  /**
   * Field that performs email validation
   */
  case class EmailField(id: String, element: String) extends Field {
    /* Assumes email address has been converted to lower case */
    private val emailRegexPattern = Pattern.compile("^[a-z0-9._%\\-+]+@(?:[a-z0-9\\-]+\\.)+[a-z]{2,4}$")
    private val message = "*Invalid e-mail address"

    override def isValid(input: String) = emailRegexPattern.matcher(input.toLowerCase).matches

    /* Feedback for the Client */
    override def jsValidation(input: String): JsCmd = {
      isValid(input) match {
        case true => ValidationSuccess(id) & ValidationSuccessFeedback(id) & ValidationMessage(id, element, "") & ValidationHideMessage(id, element)
        case false => ValidationError(id) & ValidationErrorFeedback(id) & ValidationMessage(id, element, message) & ValidationShowMessage(id, element)
      }
    }
    override def reset: JsCmd = ClearValue(id, element) & ValidationReset(id) & ValidationResetFeedback(id)
  }

  /**
   * Field that is required
   */
  case class RequiredField(id: String, element: String) extends Field {
    private val minimumLength = 1
    private val message = "*This field is required"

    override def isValid(input: String) = input match {
      case x if x.length >= minimumLength => true
      case _ => false
    }

    /* Feedback for the Client */
    override def jsValidation(input: String): JsCmd = {
      isValid(input) match {
        case true => ValidationSuccess(id) & ValidationSuccessFeedback(id) & ValidationMessage(id, element, "") & ValidationHideMessage(id, element)
        case false => ValidationError(id) & ValidationErrorFeedback(id) & ValidationMessage(id, element, message) & ValidationShowMessage(id, element)
      }
    }
    override def reset: JsCmd = ClearValue(id, element) & ValidationReset(id) & ValidationResetFeedback(id)
  }
}

case class Blur(id: String) extends JsCmd {
  def toJsCmd = "$('" + id + "').blur()"
}

import FieldValidationHelpers._

/**
 * Ajax Form
 */
class FormSnippet extends Loggable {

  /* Vars used to store input */
  var nameVar: String = ""
  var emailVar: String = ""
  var messageVar: String = ""

  /* Declare field objcts for validation */
  val nameField = RequiredField("#name", "input")
  val emailField = EmailField("#email", "input")
  val messageField = RequiredField("#message", "textarea")

  val fieldList: List[Field] = nameField :: emailField :: messageField :: Nil

  /**
   * Run when submit button is pressed
   */
  def processForm(): JsCmd = {
    logger.info("Name: " + nameVar + ", Email: " + emailVar + ", Message: " + messageVar)
    // Capture vars
    val inputList = nameVar :: emailVar :: messageVar :: Nil
    // Validate the fields
    validateFields(fieldList, inputList) match {
      // Validation Succeeded
      case None => {
        logger.info("All Fields Validated")
        // Reset Vars
        nameVar = ""
        emailVar = ""
        messageVar = ""
        // Reset fields
        nameField.reset & emailField.reset & messageField.reset & Blur("#submit")
      }
      // Validation failed.  Return JsCmds
      case Some(x) => x & Blur("#submit")
    }
  }

  /**
   * ajaxText fields store the input in a var and perform validation
   */
  def renderForm = "#name * input" #> SHtml.ajaxText(nameVar, false, (input: String) => {
    nameVar = input
    nameField.jsValidation(input)
  }) &
    "#email * input" #> SHtml.ajaxText(emailVar, false, (input: String) => {
      emailVar = input
      emailField.jsValidation(input)
    }) &
    "#message * textarea" #> SHtml.ajaxTextarea(messageVar, (input: String) => {
      messageVar = input
      messageField.jsValidation(input)
    }) &
    "#submit" #> SHtml.ajaxOnSubmit(() => processForm())
}