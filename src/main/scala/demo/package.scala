package object demo {

  type TODO = Nothing

  def TODO[A]: A = throw new Exception("not implemented")
}
