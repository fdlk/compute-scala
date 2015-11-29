import compute.model.{Step, Workflow, Protocol}
object computeWS {
  println("compute WS")
  val protocol1 = Protocol("protocols/step1.sh", List("sample"), List("project", "barcode"), List("step1.out"))
  val protocol2 = Protocol("protocols/step2.sh", List("workflowName", "creationDate"), List("step1.out"))
  val step1 = Step("step1", protocol1, Map("project" -> "project", "sample" -> "sample", "barcode" -> "barcode"))
  val step2 = Step("step1", protocol2, Map("wf" -> "workflowName", "date" -> "creationDate", "strings" -> "step1.out"))
  val ws = Workflow("wf", List(step1, step2))
  val tasksStep1 = ws.tasks(
    List(Map("project" -> "p1", "sample" -> "s1p1", "barcode" -> "b", "creationDate" -> "today", "workflowName" -> "myFirstWorkflow"),
      Map("project" -> "p1", "sample" -> "s2p1", "barcode" -> "b", "creationDate" -> "today", "workflowName" -> "myFirstWorkflow"),
      Map("project" -> "p2", "sample" -> "s1p2", "barcode" -> "b", "creationDate" -> "today", "workflowName" -> "myFirstWorkflow"),
      Map("project" -> "p2", "sample" -> "s1p2", "barcode" -> "c", "creationDate" -> "today", "workflowName" -> "myFirstWorkflow")))
  }