if stack build; then
  # python3 scripts/run_all.py
  echo "==========="
  # python3 scripts/run_all.py --topdown
  echo "==========="
  python3 scripts/run_all.py --experimental
fi

# goal: test tygar