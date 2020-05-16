package questionnaire_builder

import java.time.LocalDateTime

import scala.util.{Failure, Success, Try}

// アンケートの仕様を表す。
trait Questionnaire extends ApplicationTime {

  import Questionnaire._

  def createdUserID: CreatedUserID

  def title: Title

  def schedule: Schedule

  def sections(): Sections


  protected def setSchedule(schedule: Schedule): Questionnaire

  protected def setSections(sections: Sections): Questionnaire

  def replaceSection(section: Section): Try[Questionnaire] =
    sections().replace(section).map(setSections)


  def appendSection(add: Section): Try[Questionnaire] =
    sections().append(add).map(setSections)


  def removeSection(id: SectionIdentifier): Try[Questionnaire] =
    sections().remove(id).map(setSections)


  def changeSchedule(changed: Schedule): Try[Questionnaire] = {
    // すでに公開日をすぎている場合release時刻の変更はできない

    // 変更後のものが現在時刻より前の場合変更できない

    // 終了日は現在時刻より前にできない

    Success(setSchedule(changed))
  }

  def isReleased: Boolean = schedule.releaseTime.isAfter(now())

  def isEnd: Boolean = schedule.endTime.isAfter(now())

  def changeEndTime(time: EndTime): Try[Questionnaire] =
    Schedule(schedule.releaseTime, time).flatMap(changeSchedule)
}

object Questionnaire {
  val maxNumberOfSection: Int = 15
  val maxNumberOfTitleLength: Int = 50

  private class Time(private val time: LocalDateTime) {
    def isAfter(other: Time): Boolean = isAfter(other.time)

    def isAfter(other: LocalDateTime): Boolean = time.isAfter(other)
  }

  class CreatedUserID(val value: String)

  class Title private(val value: String)

  object Title {

    class InvalidTitleError extends Error

    def apply(value: String): Try[Title] = {
      if (value.length > maxNumberOfTitleLength) Failure(new InvalidTitleError)
      Success(new Title(value))
    }
  }

  class ReleaseTime(time: LocalDateTime) extends Time(time)

  class EndTime(time: LocalDateTime) extends Time(time)

  class Schedule(val releaseTime: ReleaseTime, val endTime: EndTime)

  object Schedule {

    class InvalidScheduleError extends Error

    def apply(releaseTime: ReleaseTime, endTime: EndTime): Try[Schedule] = {
      if (endTime.isAfter(releaseTime)) Failure(new InvalidScheduleError)
      else Success(new Schedule(releaseTime, endTime))
    }
  }

  object Sections {

    class SectionCountExceededError extends Error

  }

  trait Sections {

    import Sections._

    def toSeq: Seq[Section]

    def toMap: Map[SectionIdentifier, Section]

    def replace(replace: Section): Try[Sections]

    def findByID(id: SectionIdentifier): Option[Section]

    def append(section: Section): Try[Sections] = {
      if (count + 1 > maxNumberOfSection) Failure(new SectionCountExceededError())
      else _append(section)
    }

    protected def _append(add: Section): Try[Sections];

    def remove(id: SectionIdentifier): Try[Sections]

    def count: Int
  }

  case class SectionIdentifier(sequenceNumber: Int)

  // タイトルと入力フォームを持った区画です
  case class Section(id: SectionIdentifier, title: String, answerType: AnswerType)

  // 回答種別を表す。
  sealed trait AnswerType {
    def inputType: InputType
  }

  class FreeText extends AnswerType {
    override def inputType: InputType = TypeSingleInput
  }

  class SelectTextAnswer extends AnswerType {
    override def inputType: InputType = TypeSingleInput
  }

  class SelectOrFree extends AnswerType {
    override def inputType: InputType = TypeSingleInput
  }

  class MultiSelect extends AnswerType {
    override def inputType: InputType = TypeMultiInput
  }


  // 回答入力の種別を表す。
  sealed trait InputType {
    def emptyInput(): Input
  }

  object TypeSingleInput extends InputType {
    override def emptyInput(): Input = SingleInput.empty
  }

  object TypeMultiInput extends InputType {
    override def emptyInput(): Input = MultiInput.empty
  }

  // 回答入力を表す。
  sealed trait Input

  class SingleInput extends Input

  object SingleInput {
    def empty = new SingleInput
  }

  class MultiInput extends Input

  object MultiInput {
    def empty = new MultiInput
  }


  trait Factory {
    def create(
                userID: CreatedUserID,
                title: Title,
                schedule: Schedule,
                sections: Sections,
              ): Try[Questionnaire]
  }

}

