UPDATE files
SET content = convert_to(
  regexp_replace(
    regexp_replace(
      convert_from(content, 'UTF8'),
      E'=== (\\d{2}\\.\\d{2}\\.\\d{4} \\d{2}:\\d{2}) \\(([0-9a-f]{7,9})\\) ===',
      E'=== Deployment, git hash \\2, \\1 ===',
      'g'),
    E'=== (\\d{2}\\.\\d{2}\\.\\d{4} \\d{2}:\\d{2}) \\(([^)]+)\\) ===',
    E'=== Eintrag von \\2, \\1 ===',
    'g'),
  'UTF8')
WHERE name = 'Changelog' AND owner_id IS NULL;
