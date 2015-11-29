package compute

object model {
  type Parameter = String
  type Value = String
  type Template = String
  type ParameterCombination = Map[Parameter, Value]

  case class Task(stepName: String, template: Template, index: Int, inputValues: ParameterCombination,
                  inputList: Map[Parameter, List[Value]]) {
    def id: String = stepName + '_' + index

    override def toString = "Task[" + id + ", " + inputValues + ", " + inputList + "]\n"
  }

  case class Protocol(template: Template,
                      singleInputs: List[Parameter] = Nil,
                      listInputs: List[Parameter] = Nil,
                      outputs: List[Parameter] = Nil) {
    def listOfTuplesToMap(list: List[(Parameter, Value)]): Map[Parameter, List[Value]] = {
      Map(list.head._1 -> list.map(_._2))
    }

    def transposeInputValues(listInputValues: List[Map[Parameter, Value]]) = {
      listInputValues
        .map(_.filterKeys(listInputs.contains))
        .transpose
        .map(listOfTuplesToMap)
        .foldLeft(Map[Parameter, List[Value]]())(_ ++ _)
    }

    def tasks(stepName: String, inputValues: List[ParameterCombination]): List[Task] = {
      inputValues
        .groupBy(combination => singleInputs.flatMap(combination.get))
        .zipWithIndex
        .map({ case ((singleInputValues, listInputValues), index) =>
          Task(stepName, template, index, singleInputs.zip(singleInputValues).toMap, transposeInputValues(listInputValues))
        }).toList
    }
  }

  case class Step(name: String, protocol: Protocol, localToGlobal: Map[Parameter, Parameter]) {
    def globalToLocal(parameters: List[ParameterCombination]): List[ParameterCombination] =
      parameters.map(pc =>
        localToGlobal
          .filterKeys(localToGlobal.keySet.contains)
          .mapValues(globalName => pc.get(globalName).get)
      )

    def tasks(global: List[ParameterCombination]): Iterable[Task] = {
      protocol.tasks(name, globalToLocal(global))
    }
  }

  case class Workflow(name: String, steps: List[Step]) {

    def tasks(parameters: List[Map[Parameter, Value]]): Iterable[Task] = {
      val step = steps.head
      step.tasks(parameters)
    }
  }

}
