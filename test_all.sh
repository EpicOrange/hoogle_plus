if stack build; then
  python3 scripts/run_all.py
  echo "==========="
  python3 scripts/run_all.py --topdown
  # python3 scripts/run_all.py --experiment
fi

# goal: test tygar