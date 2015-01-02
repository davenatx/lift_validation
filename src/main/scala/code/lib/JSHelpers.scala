package code.lib

import net.liftweb.common.Loggable
import net.liftweb.http.js.JsCmd

/**
 * JavaScript for Twiter Bootstrap Feedback and Tooltips for field validation.
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

case class Blur(id: String) extends JsCmd {
  def toJsCmd = "$('" + id + "').blur()"
}