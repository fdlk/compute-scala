import compute.model.{Step, Workflow, Protocol}
object computeWS {
  println("compute WS")
  val protocol1 = Protocol("protocols/step1.sh", List("sample", "project", "barcode"), Nil, List("step1.out"))
  val protocol2 = Protocol("protocols/step2.sh", List("wf", "date"), List("strings"))
  val step1 = Step("step1", protocol1)
  val step2 = Step("step2", protocol2, Map("wf" -> "workflowName", "date" -> "creationDate", "strings" -> "step1.out"))
  val ws = Workflow("wf", List(step1, step2))
  val tasks = ws.tasks(
    List(Map("project" -> "p1", "sample" -> "s1p1", "barcode" -> "b", "creationDate" -> "today", "workflowName" -> "myFirstWorkflow"),
      Map("project" -> "p1", "sample" -> "s2p1", "barcode" -> "b", "creationDate" -> "today", "workflowName" -> "myFirstWorkflow"),
      Map("project" -> "p2", "sample" -> "s1p2", "barcode" -> "b", "creationDate" -> "today", "workflowName" -> "myFirstWorkflow"),
      Map("project" -> "p2", "sample" -> "s1p2", "barcode" -> "c", "creationDate" -> "today", "workflowName" -> "myFirstWorkflow")))
  }
