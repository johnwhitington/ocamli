let x =
  try char strm__ with
  Stream.Failure -> raise (Stream.Error "")
