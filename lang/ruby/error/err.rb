def raise_error(message)
  raise StandardError, message
end

begin
  raise_error("This is an error message.")
rescue StandardError => e
  puts "Caught an error: #{e.message}"
end