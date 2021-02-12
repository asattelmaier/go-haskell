#include <gtest/gtest.h>
#include <nlohmann/json.hpp>

using json = nlohmann::json;

TEST(Tests, sample){
  json jsonA = json::object({ { "some", "data" } });
  auto jsonB = R"({ "some": "data" })"_json;
  EXPECT_EQ(jsonA, jsonB);
}

