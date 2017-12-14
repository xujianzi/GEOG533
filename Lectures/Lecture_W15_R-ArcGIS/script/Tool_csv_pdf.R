tool_exec <- function(in_params, out_params)
{
  input <- in_params[[1]]
  output <- out_params[[1]]
  
  print("Input file is:")
  print(input)
  
  df <- read.csv(input,header = TRUE)
  pdf(output)
  plot(df)
  dev.off()
  df <- cars
  
  return (out_params)
}

