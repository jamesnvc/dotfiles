function is_func -d "Check if argument is a defined shell function"
    type $argv[1] 2>/dev/null | head -n1 | grep -q 'is a function'
end
