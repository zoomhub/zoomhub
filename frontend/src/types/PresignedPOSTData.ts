export type PresignedPOSTData = {
  "Content-Type": string;
  "x-amz-algorithm": string;
  "x-amz-credential": string;
  "x-amz-date": string;
  "x-amz-signature": string;
  bucket: string;
  key: string;
  policy: string;
  success_action_redirect: string;
  url: string;
};
