package questionnaire_builder

import java.time.format.DateTimeFormatter

import scala.util.Try


trait Answer {

  import Answer._

  def id: ID

  def writerID: WriterID

  def forSections: Seq[ForSection]

  // アンケートの形式にそった答えか否かを返します。
  def correct(): Boolean
}

class AnswerImpl(
                  val id: Answer.ID,
                  val writerID: Answer.WriterID,
                  val forSections: Seq[Answer.ForSection],
                  val correct: Boolean,
                ) extends Answer

object Answer {

  class ID(private val value: String)

  trait IDFactory extends ApplicationTime {
    def create(): ID = new ID(now().format(DateTimeFormatter.BASIC_ISO_DATE))
  }

  trait UseIDFactory {
    def idFactory: IDFactory
  }

  class WriterID(private val value: String)


  case class ForSection(id: Questionnaire.SectionIdentifier, input: Questionnaire.Input) {
    def isSatisfied(section: Questionnaire.Section): Boolean =
      section.answerType.inputType.emptyInput().getClass == input.getClass
  }

  class AnswerCreationError extends Error

  trait Factory extends UseIDFactory {
    def create(
                writerID: WriterID,
                questionnaire: Questionnaire,
                forSections: Seq[Answer.ForSection]
              ): Try[Answer] = {
      val isCorrectT = Try[Boolean] {
        val sectionVerificationResults: Seq[Boolean] = for (forSection <- forSections) yield {
          val sectionOpt = questionnaire.sections().findByID(forSection.id)
          sectionOpt match {
            case Some(section) =>
              !forSection.isSatisfied(section)
            case None =>
              throw new AnswerCreationError()
          }
        }
        sectionVerificationResults.forall(_ == true)
      }

      for (isCorrect <- isCorrectT) yield {
        new AnswerImpl(
          idFactory.create(),
          writerID,
          forSections,
          isCorrect,
        )
      }
    }
  }
}
