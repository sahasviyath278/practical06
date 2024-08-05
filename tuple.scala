

// Main application
object StudentRecordApp extends App {
  import scala.io.StdIn.readLine

  // Function to assign grade based on percentage
  def assignGrade(percentage: Double): Char = {
    percentage match {
      case p if p >= 90 => 'A'
      case p if p >= 75 => 'B'
      case p if p >= 50 => 'C'
      case _ => 'D'
    }
  }

  // Function to get student info
  def getStudentInfo: (String, Int, Int, Double, Char) = {
    println("Enter student's name:")
    val name = readLine().trim

    println("Enter marks obtained:")
    val marks = readLine().trim.toInt

    println("Enter total possible marks:")
    val totalMarks = readLine().trim.toInt

    // Validate input
    validateInput(name, marks, totalMarks) match {
      case (true, _) =>
        val percentage = (marks.toDouble / totalMarks) * 100
        val grade = assignGrade(percentage)
        (name, marks, totalMarks, percentage, grade)
      case (false, Some(errorMessage)) =>
        println(s"Invalid input: $errorMessage")
        throw new IllegalArgumentException("Invalid input data")
      case _ =>
        throw new RuntimeException("Unexpected validation outcome")
    }
  }

  // Function to print student record
  def printStudentRecord(record: (String, Int, Int, Double, Char)): Unit = {
    val (name, marks, totalMarks, percentage, grade) = record
    println(s"Name: $name")
    println(s"Marks: $marks out of $totalMarks")
    println(f"Percentage: $percentage%.2f%%")
    println(s"Grade: $grade")
  }

  // Function to validate input
  def validateInput(name: String, marks: Int, totalMarks: Int): (Boolean, Option[String]) = {
    if (name.isEmpty) {
      (false, Some("Name cannot be empty"))
    } else if (marks < 0 || totalMarks <= 0 || marks > totalMarks) {
      (false, Some("Marks must be a non-negative integer and not exceed total possible marks"))
    } else {
      (true, None)
    }
  }

  // Function to get student info with retry on invalid input
  def getStudentInfoWithRetry: (String, Int, Int, Double, Char) = {
    var studentInfo: (String, Int, Int, Double, Char) = null
    var valid = false

    while (!valid) {
      try {
        studentInfo = getStudentInfo
        valid = true
      } catch {
        case _: IllegalArgumentException => // continue to retry
      }
    }

    studentInfo
  }
  val studentRecord = getStudentInfoWithRetry
  printStudentRecord(studentRecord)
}

