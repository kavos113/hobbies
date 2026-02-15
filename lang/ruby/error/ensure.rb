def raise_error(message)
  raise StandardError, message
end

def not_raise_error(message)
  begin
    puts "not raise an error"
  rescue StandardError => e
    puts "Caught an error: #{e.message}"
  ensure
    puts "This will always be executed."
  end
end

def raise_error_with_ensure(message)
  begin
    raise_error(message)
  rescue StandardError => e
    puts "Caught an error: #{e.message}"
  ensure
    puts "This will always be executed."
  end
end

def ensure_with_raise_in_rescue(message)
  begin
    raise_error(message)
  rescue StandardError => e
    puts "Caught an error: #{e.message}"
    raise "Raising a new error in rescue block."
  ensure
    puts "This will always be executed, even if a new error is raised."
    puts "error: #{e.message}" if defined?(e)
  end
end

not_raise_error("This is an error message.")
puts "-----------------------------"
raise_error_with_ensure("This is an error message.")
puts "-----------------------------"
ensure_with_raise_in_rescue("This is an error message.")