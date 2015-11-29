package compute

import model.Workflow
import model.Step


object computeWS {
  val step1 = Step("step1", "protocols/step1.sh", List("sample"), List("project", "barcode"), List("step1.out"))
                                                  //> step1  : compute.model.Step = Step(step1,protocols/step1.sh,List(sample),Lis
                                                  //| t(project, barcode),List(step1.out))
  val step2 = Step("step2", "protocols/step2.sh", List("workflowName", "creationDate"), List("step1.out"))
                                                  //> step2  : compute.model.Step = Step(step2,protocols/step2.sh,List(workflowNam
                                                  //| e, creationDate),List(step1.out),List())
  val ws = Workflow("wf", List(step1, step2))     //> ws  : compute.model.Workflow = Workflow(wf,List(Step(step1,protocols/step1.s
                                                  //| h,List(sample),List(project, barcode),List(step1.out)), Step(step2,protocols
                                                  //| /step2.sh,List(workflowName, creationDate),List(step1.out),List())))
  ws.tasks(
    List(Map("project" -> "p1", "sample" -> "s1p1", "barcode" -> "b", "creationDate" -> "today", "workflowName" -> "myFirstWorkflow"),
      Map("project" -> "p1", "sample" -> "s2p1", "barcode" -> "b", "creationDate" -> "today", "workflowName" -> "myFirstWorkflow"),
      Map("project" -> "p2", "sample" -> "s1p2", "barcode" -> "b", "creationDate" -> "today", "workflowName" -> "myFirstWorkflow"),
      Map("project" -> "p2", "sample" -> "s1p2", "barcode" -> "c", "creationDate" -> "today", "workflowName" -> "myFirstWorkflow")))
                                                  //> res0: Iterable[compute.model.Task] = List(Task[step1_0, List(s1p1), List(Lis
                                                  //| t(b, p1))], Task[step1_1, List(s1p2), List(List(b, p2), List(c, p2))], Task[
                                                  //| step1_2, List(s2p1), List(List(b, p1))])
      
}