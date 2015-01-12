package code.snippet

import net.liftweb.util._
import net.liftweb.common.Loggable
import Helpers._
import net.liftweb.http.SHtml
import net.liftweb.http.js.JsCmd

import code.lib.FieldValidationHelpers._
import code.lib.Blur
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
    nameVar = input.trim
    nameField.jsValidation(input)
  }) &
    "#email * input" #> SHtml.ajaxText(emailVar, false, (input: String) => {
      emailVar = input.trim
      emailField.jsValidation(input)
    }) &
    "#message * textarea" #> SHtml.ajaxTextarea(messageVar, (input: String) => {
      messageVar = input.trim
      messageField.jsValidation(input)
    }) &
    "#submit" #> SHtml.ajaxOnSubmit(() => processForm())
}